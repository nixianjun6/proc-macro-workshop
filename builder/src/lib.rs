use quote::{format_ident, quote};
use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput, Data, Type, TypePath, PathArguments, GenericArgument};

fn inner_of_type(field_type: &Type, str: String) ->std::option::Option<Type> {
    if let Type::Path(TypePath { qself: None, path }) = field_type {
        if let Some(segment) = path.segments.last() {
            if segment.ident.to_string() == str {
                if let PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(GenericArgument::Type(inner_type)) = args.args.first() {
                        return Some(inner_type.clone())
                    }
                }
            }
        }
    }
    None
}

enum ParseBuilderAttributeResult {
    Valid(String),
    Invalid(syn::Meta),
}

fn unwrap_builder_attr_value(attrs: &[syn::Attribute]) -> std::option::Option<ParseBuilderAttributeResult> {
    attrs.iter().find_map(|attr| {
        if attr.path().is_ident("builder") {
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
                if !path.is_ident("each") {
                    return Some(ParseBuilderAttributeResult::Invalid(attr.meta.clone()));
                }
                return Some(ParseBuilderAttributeResult::Valid(liststr.value()));
            } else {
                return None;
            }
        }

        None
    })
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let parse_input = parse_macro_input!(input as DeriveInput);

    let struct_name = &parse_input.ident;

    if let Data::Struct(data_struct) = &parse_input.data {
        let builder_fields = data_struct.fields.iter().map(|field| {
            let field_name = &field.ident;
            let field_type = &field.ty;
            
            if let Some(inner_type) = inner_of_type(field_type, "Option".to_string()) {
                return quote! {
                    #field_name: std::option::Option<#inner_type>,
                };
            } 

            quote! {
                #field_name: std::option::Option<#field_type>,
            }
        });

        let builder_init = data_struct.fields.iter().map(|field| {
            let field_name = &field.ident;
            quote! {
                #field_name: None,
            }
        });

        let builder_methods = data_struct.fields.iter().map(|field| {
            let field_name = &field.ident;
            let field_type = &field.ty;

            //check option
            if let Some(inner_type) = inner_of_type(field_type, "Option".to_string()) {
                return quote! {
                    pub fn #field_name(&mut self, #field_name: #inner_type) -> &mut Self {
                        self.#field_name = Some(#field_name);
                        self
                    }
                };
            }

            match unwrap_builder_attr_value(&field.attrs) {
                Some(ParseBuilderAttributeResult::Valid(each)) => {
                    let each_ident = format_ident!("{}", each);
                    if let Some(inner_type) = inner_of_type(field_type, "Vec".to_string()) {
                        let vec_setters = quote! {
                            fn #each_ident(&mut self, #each_ident: #inner_type) -> &mut Self {
                                if let Some(ref mut values) = self.#field_name {
                                    values.push(#each_ident);
                                } else {
                                    self.#field_name = std::option::Option::Some(vec![#each_ident]);
                                }
                                self
                            }
                        };

                        if field_name.clone().unwrap() == each_ident {
                            return vec_setters;
                        } else {
                            return quote! {
                                #vec_setters

                                pub fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                                    self.#field_name = Some(#field_name);
                                    self
                                }
                            };
                        }
                    } else {
                        quote! {}
                    }
                }

                Some(ParseBuilderAttributeResult::Invalid(meta)) => {
                    syn::Error::new_spanned(meta, "expected `builder(each = \"...\")`")
                            .to_compile_error()
                            .into()
                }

                None => {
                    quote! {
                        pub fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                            self.#field_name = Some(#field_name);
                            self
                        }
                    }
                }
            }
        });
        
        let builder_build = {
            let construct_fields = data_struct.fields.iter().map(|field| {
                let field_name = &field.ident;
                let field_type = &field.ty;

                // check option
                if let Some(_) = inner_of_type(field_type, "Option".to_string()) {
                    return quote! {
                        #field_name: self.#field_name.take(),
                    };
                } 

                if let Some(_) = inner_of_type(field_type, "Vec".to_string()) {
                    return quote! {
                        #field_name: self.#field_name.take().unwrap_or_else(Vec::new),
                    };
                } 

                quote! {
                    #field_name: self.#field_name.take().unwrap(),
                }
            });

            quote! {
                Ok(Command {
                    #(#construct_fields)*
                })
            }
        };

        let expanded = quote! {
            pub struct CommandBuilder {
                #(#builder_fields)*
            }

            impl CommandBuilder {
                #(#builder_methods)*

                pub fn build(&mut self) -> std::result::Result<Command, std::boxed::Box<dyn std::error::Error>> {
                    #builder_build
                }
            }

            impl #struct_name {
                pub fn builder() -> CommandBuilder {
                    CommandBuilder {
                        #(#builder_init)*
                    }
                }
            }
        };

        expanded.into()
    } else {
        TokenStream::new()
    }
}