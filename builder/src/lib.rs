use proc_macro::TokenStream;
use quote::{quote, format_ident};
use syn::{parse_macro_input, DeriveInput, Data, parse_quote, Field};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let builder_name = format_ident!("{}Builder", input.ident);

    let fields = match &input.data {
        Data::Struct(s) => &s.fields,
        _ => panic!("Expected struct")
    };

    let builder_fields = fields.iter().map(|field| {
        let mut new_field = field.clone();
        let ty = new_field.ty;
        new_field.ty = parse_quote! {
            core::option::Option<#ty>
        };
        new_field
    }).collect::<Vec<_>>();

    let builder_init_nones = fields.iter().map(|field| {
        let ident = &field.ident;
        quote! { #ident: None }
    });

    let builder_setters = fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;

        quote! {
            pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        }
        
    });

    let builder_build_body = fields.iter().map(|field| {
        let ident = &field.ident;

        let err_msg = format!("missing field '{}'", ident.as_ref().unwrap());

        quote! {
            let #ident = self.#ident.take().ok_or(#err_msg)?;
        }
    });

    let field_names = fields.iter().map(|field| field.ident.as_ref());

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
                #(#builder_build_body)*
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
