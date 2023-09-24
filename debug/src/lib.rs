use quote::quote;
use proc_macro::TokenStream;
use syn::{parse_macro_input, spanned::Spanned, DeriveInput};
use syn::visit::{self, Visit};

fn extract_debug_attributes(
    attrs: &[syn::Attribute],
) -> Result<Vec<String>, proc_macro2::TokenStream> {
    let mut attrs_values = vec![];

    for attr in attrs {
        match &attr.meta {
            syn::Meta::NameValue(ref named) if named.path.is_ident("debug") => {
                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(litstr),
                    ..
                }) = &named.value
                {
                    attrs_values.push(litstr.value());
                }
            }
            _ => {
                return Err(
                    syn::Error::new(attr.span(), "only debug attributes can be applied")
                        .to_compile_error(),
                )
            }
        }
    }
    Ok(attrs_values)
}

fn get_field_type_name(field: &syn::Field) -> Option<String> {
    if let syn::Type::Path(syn::TypePath{path: syn::Path{ref segments, ..}, ..}) = field.ty {
        if let Some(syn::PathSegment{ref ident,..}) = segments.last() {
            return Some(ident.to_string());
        }
    }
    None
}

fn get_phantomdata_generic_type_name(field: &syn::Field) -> Option<String> {
    if let syn::Type::Path(syn::TypePath{path: syn::Path{ref segments, ..}, ..}) = field.ty {
        if let Some(syn::PathSegment{ref ident, ref arguments}) = segments.last() {
            if ident == "PhantomData" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{args, ..}) = arguments {
                    if let Some(syn::GenericArgument::Type(syn::Type::Path( ref gp))) = args.first() {
                        if let Some(generic_ident) = gp.path.segments.first() {
                            return Some(generic_ident.ident.to_string());
                        }
                    }
                }
            }
        }
    }
    None
}

struct TypePathVisitor {
    generic_type_names: Vec<String>,
    associated_types: std::collections::HashMap<String, Vec<syn::TypePath>>, 
}

impl<'ast> Visit<'ast> for TypePathVisitor {
    fn visit_type_path(&mut self, node: &'ast syn::TypePath) {
        
        if node.path.segments.len() >= 2 {
            let generic_type_name = node.path.segments[0].ident.to_string();
            if self.generic_type_names.contains(&generic_type_name) {
                self.associated_types.entry(generic_type_name).or_insert(Vec::new()).push(node.clone());
            }
        }
        visit::visit_type_path(self, node);
    }
}

fn unwrap_debug_attr_value(attrs: &[syn::Attribute]) -> std::option::Option<String> {
    attrs.iter().find_map(|attr| {
        if attr.path().is_ident("debug") {
            if let Ok(syn::MetaNameValue {
                value:
                    syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(ref liststr),
                        ..
                    }),
                path,
                ..
            }) = attr.parse_args::<syn::MetaNameValue>()
            {
                if !path.is_ident("bound") {
                    return None;
                }
                return Some(liststr.value());
            } else {
                return None;
            }
        }

        None
    })
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let parse_input = parse_macro_input!(input as DeriveInput);

    let struct_name = &parse_input.ident;

    if let syn::Data::Struct(data_struct) = &parse_input.data {

        let mut field_type_names = Vec::new();
        let mut phantomdata_type_param_names = Vec::new();

        if let syn::Fields::Named(syn::FieldsNamed { ref named, .. }) = data_struct.fields {
            for field in named {
                if let Some(field_type_string) = get_phantomdata_generic_type_name(field) {
                    phantomdata_type_param_names.push(field_type_string);
                } else if let Some(field_type_string) = get_field_type_name(&field) {
                    field_type_names.push(field_type_string);
                }
            };
        }

        let origin_generic_param_names: Vec<String> = parse_input.generics.params.iter().filter_map(|f| {
            if let syn::GenericParam::Type(ty) = f {
                return Some(ty.ident.to_string())
            }
            return None
        }).collect();

        let mut visitor = TypePathVisitor {
            generic_type_names: origin_generic_param_names,
            associated_types: std::collections::HashMap::new(),
        };

        visitor.visit_derive_input(&parse_input);
        let associated_types_map = visitor.associated_types;


        let struct_debug = data_struct.fields.iter().map(|field| {
            let field_name = &field.ident;

            match extract_debug_attributes(&field.attrs) {
                Ok(values) => match values.first() {
                    Some(attr) => {
                        quote! {
                            .field(
                                stringify!(#field_name),
                                &format_args!(#attr, &self.#field_name))
                        }
                    }
                    None => {
                        quote! {
                            .field(stringify!(#field_name), &self.#field_name)
                        }
                    }
                },
                Err(err) => err.into(),
            }
        });

        let mut generics_param_to_modify = parse_input.generics.clone();

        if let Some(hatch) = unwrap_debug_attr_value(&parse_input.attrs) {
            generics_param_to_modify.make_where_clause();
            generics_param_to_modify
                        .where_clause
                        .as_mut()
                        .unwrap()
                        .predicates
                        .push(syn::parse_str(hatch.as_str()).unwrap());
        } else {
            for g in generics_param_to_modify.params.iter_mut() {
                if let syn::GenericParam::Type(t) = g {
                    let type_param_name = t.ident.to_string();
                    if phantomdata_type_param_names.contains(&type_param_name) && !field_type_names.contains(&type_param_name) {
                        continue;
                    }

                    if associated_types_map.contains_key(&type_param_name) && !field_type_names.contains(&type_param_name){
                        continue
                    }
                    t.bounds.push(syn::parse_quote!(std::fmt::Debug));
                }
            }

            generics_param_to_modify.make_where_clause();
            for (_, associated_types) in associated_types_map {
                for associated_type in associated_types {
                    generics_param_to_modify.where_clause.as_mut().unwrap().predicates.push(syn::parse_quote!(#associated_type:std::fmt::Debug));
                }
            }
        }
        let (impl_generics, type_generics, where_clause) = generics_param_to_modify.split_for_impl();

        let expanded = quote! {
            impl #impl_generics std::fmt::Debug for #struct_name #type_generics #where_clause
            {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.debug_struct(stringify!(#struct_name))
                    #(#struct_debug)*
                    .finish()
                }
            }
        };

        return expanded.into();
    }
    TokenStream::new()
}
