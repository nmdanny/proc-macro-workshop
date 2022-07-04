use std::collections::HashSet;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, visit::Visit, Data,
    DataStruct, DeriveInput, Field, Generics, Lit, MetaNameValue,
    TypeParam, TypeParamBound,
};

struct CustomDebugField<'ast> {
    field: &'ast Field,
    format_string: Option<String>,
}

struct CustomDebugBuilder<'ast> {
    fields: Vec<CustomDebugField<'ast>>,
    ident: &'ast Ident,
    data: &'ast Data,
    generics: &'ast Generics,
}

impl<'ast> CustomDebugBuilder<'ast> {
    fn new(input: &'ast DeriveInput) -> syn::Result<Self> {
        let ident = &input.ident;
        let data = &input.data;
        let generics = &input.generics;
        let mut builder = Self {
            data,
            ident,
            generics,
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

    fn get_adjusted_generics(&self) -> Generics {
        let mut generics = self.generics.clone();
        let all_generic_idents = self
            .generics
            .type_params()
            .map(|tp| tp.ident.to_string())
            .collect();
        let mut used_params = HashSet::<String>::new();
        for field in &self.fields {
            let field_params = non_phantom_generics(&all_generic_idents, &field.field.ty);
            used_params.extend(field_params.into_iter());
        }
        for param in generics.type_params_mut() {
            if used_params.contains(&param.ident.to_string()) {
                ensure_has_debug(param);
            }
        }
        generics
    }

    fn build(self) -> syn::Result<proc_macro2::TokenStream> {
        let ident = self.ident;
        let ident_s = ident.to_string();
        let fmt_begin = match self.data {
            Data::Struct(_) => quote! { fmt.debug_struct(#ident_s)},
            Data::Enum(_) => todo!(),
            Data::Union(_) => todo!(),
        };
        let fmt_fields = self.fields.iter().map(|field| {
            let field_ident = field
                .field
                .ident
                .as_ref()
                .expect("TODO fields without ident(tuple)");
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
        let generics = self.get_adjusted_generics();
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
        Ok(quote! {
            impl #impl_generics std::fmt::Debug for #ident #ty_generics #where_clause {
                fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    #fmt_begin
                    #(#fmt_fields)*
                       .finish()
                }
            }
        })
    }
}

fn ensure_has_debug(param: &mut TypeParam) {
    let has_debug = param
        .bounds
        .iter()
        .find_map(|bound| match bound {
            TypeParamBound::Trait(t)
                if t.path.get_ident().map(|i| i.to_string()) == Some("Debug".into()) =>
            {
                Some(t)
            }
            _ => None,
        })
        .is_some();
    if !has_debug {
        param.bounds.push(parse_quote!(std::fmt::Debug));
    }
}

fn non_phantom_generics<'ast>(
    all_generics: &'ast HashSet<String>,
    ty: &'ast syn::Type,
) -> HashSet<String> {
    struct Visitor<'ast> {
        all_generics: &'ast HashSet<String>,
        set: HashSet<String>,
    }
    impl<'ast> Visit<'ast> for Visitor<'ast> {
        fn visit_type_path(&mut self, i: &'ast syn::TypePath) {
            let continue_visit = i
                .path
                .segments
                .iter()
                .find(|seg| seg.ident.to_string() == "PhantomData")
                .is_none();
            if let Some(ident) = i.path.get_ident() {
                let ident = ident.to_string();
                if self.all_generics.contains(&ident) {
                    self.set.insert(ident);
                }
            }
            if continue_visit {
                syn::visit::visit_type_path(self, i);
            }
        }
    }
    let mut visitor = Visitor {
        set: HashSet::new(),
        all_generics,
    };
    visitor.visit_type(ty);
    visitor.set
}

impl<'ast> std::fmt::Debug for CustomDebugBuilder<'ast> {
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


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_non_phantom_generics() {
        let ty: syn::Type = parse_quote!(Foo<T, (X,Y, PhantomData<B>)>);
        let all_generics = ["T", "X", "Y", "B"]
            .into_iter()
            .map(|s| s.to_owned())
            .collect::<HashSet<_>>();
        let ty_param_set = non_phantom_generics(&all_generics, &ty);
        let ty_idents = ty_param_set
            .iter()
            .map(|ident| ident.to_string())
            .collect::<HashSet<_>>();
        assert_eq!(
            ty_idents,
            HashSet::from(["T", "X", "Y"].map(|s| s.to_owned()))
        );
    }
}
