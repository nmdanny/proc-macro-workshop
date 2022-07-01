use proc_macro::TokenStream;
use quote::{quote, format_ident, ToTokens};
use syn::{parse_macro_input, DeriveInput, Data, parse_quote, Field, Type, Path, TypePath, Ident, AngleBracketedGenericArguments, GenericArgument};


struct BuilderField<'f> {
    field: &'f Field
}

impl <'f> BuilderField<'f> {
    fn new(field: &'f Field) -> Self {
        BuilderField { field }
    }

    fn ident(&self) -> &syn::Ident {
        self.field.ident.as_ref().unwrap()
    }

    fn ty(&self) -> &syn::Type {
        &self.field.ty
    }

    fn is_option(&self) -> bool {
        self.option_of().is_some()
    }

    fn option_of(&self) -> Option<&syn::Type> {
        let ty = &self.field.ty;
        match ty {
            Type::Path(TypePath {
                qself: None,
                path: Path {
                    segments,
                    ..
                }
            }) => {
                let first_segment = segments.first()?;
                (first_segment.ident.to_string() == "Option").then_some(())?;
                match &first_segment.arguments {
                    syn::PathArguments::AngleBracketed(
                        AngleBracketedGenericArguments {
                            args,
                            ..
                        }
                    ) => {
                        let first_arg = args.first().expect("Option should have one type argument");
                        if let GenericArgument::Type(ty) = first_arg {
                            return Some(ty);
                        }
                    }
                    _ => panic!("Expected option to be angle bracketed")
                }

            },
            _ => return None,
        }
        None
    }


    fn to_field(&self) -> Field {
        let mut new_field = self.field.clone();
        if self.is_option() {
            return new_field
        }
        let ty = self.ty();
        new_field.ty = parse_quote! {
            core::option::Option<#ty>
        };
        new_field
    }

    fn to_setter(&self) -> impl ToTokens {
        let ident = self.ident();
        let ty = self.ty();

        if let Some(underlying_ty) = self.option_of() {
            quote ! {
                pub fn #ident(&mut self, #ident: #underlying_ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        } else {
            quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        }

    }


    fn to_build_body(&self) -> impl ToTokens {
        let ident = self.ident();
        let err_msg = format!("missing field '{}'", ident);

        if self.is_option() {
            quote! {
                let #ident = self.#ident.take();
            }
        } else {
            quote! {
                let #ident = self.#ident.take().ok_or(#err_msg)?;
            }
        }

    }

    fn to_init_none(&self) -> impl ToTokens {
        let ident = self.ident();
        quote! { #ident: None }
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let builder_name = format_ident!("{}Builder", input.ident);

    let fields = match &input.data {
        Data::Struct(ref s) => s.fields
            .iter()
            .map(BuilderField::new)
            .collect::<Vec<_>>(),
        _ => panic!("Expected struct")
    };

    let builder_fields = fields.iter().map(BuilderField::to_field);


    let builder_init_nones = fields.iter().map(BuilderField::to_init_none);
    let builder_setters = fields.iter().map(BuilderField::to_setter);
    let builder_build_bodies = fields.iter().map(BuilderField::to_build_body);
    let field_names = fields.iter().map(BuilderField::ident);

    let struct_name = input.ident;
    let expanded = quote! {
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_init_nones),*
                }
            }
        }
        pub struct #builder_name {
            #(#builder_fields),*
        }

        impl #builder_name {
            #(#builder_setters)*

            pub fn build(&mut self) -> core::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
                #(#builder_build_bodies)*
                Ok(#struct_name {
                    #(#field_names),*
                })
            }
        }
    };
    let res = TokenStream::from(expanded);
    eprintln!("TOKENS: {}", res);
    res
}
