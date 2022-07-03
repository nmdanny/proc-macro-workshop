use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{quote};
use syn::{
    parse_macro_input, Data, DataStruct, DeriveInput, Field, Lit,
    MetaNameValue,
};

struct CustomDebugField<'ast> {
    field: &'ast Field,
    format_string: Option<String>,
}

impl<'ast> CustomDebugBuilder<'ast> {
    fn new(input: &'ast DeriveInput) -> syn::Result<Self> {
        let ident = &input.ident;
        let data = &input.data;
        let mut builder = Self {
            data, ident,
            fields: Vec::new(),
        };
        if let Some(data_struct) = builder.as_struct() {
            for field in &data_struct.fields {
                builder.add_field(field)?;
            }
        }
        Ok(builder)
    }

    fn add_field(&mut self, field: &'ast Field) -> syn::Result<&mut Self> {
        let meta = field
            .attrs
            .iter()
            .find(|a| a.path.get_ident().map(|i| i.to_string()) == Some("debug".into()))
            .map(|a| a.parse_meta());
        let format_string = if let Some(meta_parse_res) = meta {
            let meta = meta_parse_res?;
            match meta {
                syn::Meta::NameValue(MetaNameValue {
                    lit: Lit::Str(s), ..
                }) => Some(s.value()),
                _ => return Err(syn::Error::new_spanned(meta, "Expected format string")),
            }
        } else {
            None
        };
        let field = CustomDebugField {
            field,
            format_string,
        };
        self.fields.push(field);
        Ok(self)
    }

    fn as_struct(&self) -> Option<&'ast DataStruct> {
        match self.data {
            syn::Data::Struct(s) => Some(s),
            _ => None,
        }
    }

    fn build(self) -> syn::Result<proc_macro2::TokenStream> {
        let ident = self.ident;
        let ident_s = ident.to_string();
        let fmt_begin = match self.data {
            Data::Struct(_) => quote! { fmt.debug_struct(#ident_s)},
            Data::Enum(_) => todo!(),
            Data::Union(_) => todo!(),
        };
        let fmt_fields = self.fields.iter()
            .map(|field| {
                let field_ident = field.field.ident.as_ref().expect("TODO fields without ident(tuple)");
                let field_name = field_ident.to_string();
                let debuggable = match &field.format_string {
                    Some(fmt_string) => {
                        quote! {
                            &format_args!(#fmt_string, &self.#field_ident)
                        }
                    }
                    None => quote! { &self.#field_ident },
                };
                quote! { .field(#field_name, #debuggable)}
            });
        Ok(quote! {
            impl std::fmt::Debug for #ident {
                fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    #fmt_begin
                    #(#fmt_fields)*
                       .finish()
                }
            }
        })
    }
}

struct CustomDebugBuilder<'ast> {
    fields: Vec<CustomDebugField<'ast>>,
    ident: &'ast Ident,
    data: &'ast Data,
}

impl <'ast> std::fmt::Debug for CustomDebugBuilder<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = f; 
        todo!()
    }
}

fn run_derive(input: DeriveInput) -> syn::Result<proc_macro::TokenStream> {
    let builder = CustomDebugBuilder::new(&input)?;
    Ok(builder.build()?.into())
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    run_derive(input).unwrap_or_else(|syn_err| syn_err.to_compile_error().into())
}
