use quote::quote;
use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput, Data, Type, TypePath, PathArguments, GenericArgument};

fn inner_of_opion(field_type: &Type) ->Option<Type> {
    if let Type::Path(TypePath { qself: None, path }) = field_type {
        if let Some(segment) = path.segments.last() {
            if segment.ident.to_string() == "Option" {
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

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let parse_input = parse_macro_input!(input as DeriveInput);

    let struct_name = &parse_input.ident;

    if let Data::Struct(data_struct) = &parse_input.data {
        let builder_fields = data_struct.fields.iter().map(|field| {
            let field_name = &field.ident;
            let field_type = &field.ty;
            
            if let Some(inner_type) = inner_of_opion(field_type) {
                return quote! {
                    #field_name: Option<#inner_type>,
                };
            } 

            quote! {
                #field_name: Option<#field_type>,
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
            if let Some(inner_type) = inner_of_opion(field_type) {
                return quote! {
                    pub fn #field_name(&mut self, #field_name: #inner_type) -> &mut Self {
                        self.#field_name = Some(#field_name);
                        self
                    }
                };
            } 
            
            quote! {
                pub fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                    self.#field_name = Some(#field_name);
                    self
                }
            }
            
        });
        
        let builder_build = {
            let construct_fields = data_struct.fields.iter().map(|field| {
                let field_name = &field.ident;
                let field_type = &field.ty;

                // check option
                if let Some(_) = inner_of_opion(field_type) {
                    return quote! {
                        #field_name: self.#field_name.take(),
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

                pub fn build(&mut self) -> Result<Command, Box<dyn Error>> {
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