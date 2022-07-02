use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, visit::Visit, Attribute, Data, DeriveInput, Field,
    GenericArgument, Meta, MetaNameValue, Type, TypePath,
};

enum FieldType<'f> {
    Regular,
    Optional(&'f Type),
    Each { field: String, underlying: &'f Type },
}

struct BuilderField<'f> {
    field: &'f Field,
    f_type: FieldType<'f>,
}

fn find_each_attribute(attrs: &[Attribute]) -> Option<syn::Result<String>> {
    let builder_attr = attrs.iter().find(|attr| attr.path.is_ident("builder"))?;
    let meta = builder_attr
        .parse_meta()
        .expect("Couldn't parse meta from attribute");
    struct Visitor<'ast> {
        inside_each: bool,
        each: Option<syn::Result<String>>,
        meta: &'ast Meta,
    }
    impl<'ast> Visit<'ast> for Visitor<'ast> {
        fn visit_meta(&mut self, i: &'ast Meta) {
            self.inside_each = match i {
                Meta::NameValue(MetaNameValue { path, .. }) if path.is_ident("each") => true,
                Meta::NameValue(_) => {
                    self.each = Some(Err(syn::Error::new_spanned(
                        self.meta,
                        "expected `builder(each = \"...\")`",
                    )));
                    return;
                }
                _ => false,
            };
            syn::visit::visit_meta(self, i);
        }

        fn visit_lit_str(&mut self, i: &'ast syn::LitStr) {
            if self.inside_each {
                self.each = Some(Ok(i.value()));
            }
        }
    }
    let mut visitor = Visitor {
        inside_each: false,
        each: None,
        meta: &meta,
    };
    visitor.visit_meta(&meta);
    visitor.each
}

fn find_underlying<'ast>(ty: &'ast Type, outer_name: &'ast str) -> Option<&'ast Type> {
    struct Visitor<'ast> {
        inside_outer: bool,
        outer_name: &'ast str,
        underlying: Option<&'ast Type>,
    }

    impl<'ast> syn::visit::Visit<'ast> for Visitor<'ast> {
        fn visit_type(&mut self, i: &'ast Type) {
            if let Type::Path(TypePath { path, .. }) = i {
                self.inside_outer = path.segments.first().map(|s| s.ident.to_string())
                    == Some(self.outer_name.into())
            }
            if self.underlying.is_none() {
                syn::visit::visit_type(self, i);
            }
        }

        fn visit_path_arguments(&mut self, i: &'ast syn::PathArguments) {
            match i {
                syn::PathArguments::AngleBracketed(ab)
                    if self.inside_outer && self.underlying.is_none() =>
                {
                    let first_arg = ab.args.first();
                    if let Some(GenericArgument::Type(ty)) = first_arg {
                        self.underlying = Some(ty);
                    }
                }
                _ => {}
            }
        }
    }
    let mut visitor = Visitor {
        inside_outer: false,
        outer_name,
        underlying: None,
    };
    visitor.visit_type(ty);
    visitor.underlying
}

impl<'f> BuilderField<'f> {
    fn new(field: &'f Field) -> syn::Result<Self> {
        let ty = &field.ty;
        let mut f_type = FieldType::Regular;
        if let Some(each) = find_each_attribute(&field.attrs) {
            let each = each?;
            let underlying =
                find_underlying(ty, "Vec").expect("Type of an 'each' field must be Vec");
            f_type = FieldType::Each {
                field: each,
                underlying,
            }
        } else if let Some(underlying) = find_underlying(ty, "Option") {
            f_type = FieldType::Optional(underlying);
        }
        Ok(BuilderField { field, f_type })
    }

    fn ident(&self) -> &syn::Ident {
        self.field.ident.as_ref().unwrap()
    }

    fn ty(&self) -> &syn::Type {
        &self.field.ty
    }

    fn to_field(&self) -> Field {
        let mut new_field = self.field.clone();
        new_field.attrs.clear();
        match self.f_type {
            // For a regular field, wrap the type in an Option
            FieldType::Regular => {
                let ty = self.ty();
                new_field.ty = parse_quote! {
                    std::option::Option<#ty>
                };
                new_field
            }

            // Optional or each fields are already wrapped in an appropriate collection
            _ => new_field,
        }
    }

    fn to_setter(&self) -> impl ToTokens {
        let ident = self.ident();
        let ty = self.ty();

        match &self.f_type {
            FieldType::Regular => quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            },
            FieldType::Optional(underlying) => quote! {
                pub fn #ident(&mut self, #ident: #underlying) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            },
            FieldType::Each { field, underlying } => {
                let all_setter = quote! {
                    pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = #ident;
                        self
                    }
                };
                let each_setter_fn = format_ident!("{}", field);
                let each_setter = quote! {
                    pub fn #each_setter_fn(&mut self, #ident: #underlying) -> &mut Self {
                        self.#ident.push(#ident);
                        self
                    }
                };
                if ident.to_string() != field.to_string() {
                    eprintln!("Making both setters");
                    quote! {
                        #all_setter
                        #each_setter
                    }
                } else {
                    quote! {
                        #each_setter
                    }
                }
            }
        }
    }

    fn to_build_body(&self) -> impl ToTokens {
        let ident = self.ident();
        let err_msg = format!("missing field '{}'", ident);

        match &self.f_type {
            FieldType::Regular => quote! {
                let #ident = self.#ident.take().ok_or(#err_msg)?;
            },
            FieldType::Optional(_) => quote! {
                let #ident = self.#ident.take();
            },
            FieldType::Each { .. } => quote! {
                let #ident = self.#ident.drain(..).collect();
            },
        }
    }

    fn to_init_body(&self) -> impl ToTokens {
        let ident = self.ident();
        match &self.f_type {
            FieldType::Regular | FieldType::Optional(_) => quote! { #ident: std::option::Option::None },
            FieldType::Each { .. } => quote! { #ident: std::vec::Vec::new() },
        }
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let builder_name = format_ident!("{}Builder", input.ident);

    let fields = match &input.data {
        Data::Struct(ref s) => {
            let res = s
                .fields
                .iter()
                .map(BuilderField::new)
                .collect::<syn::Result<Vec<_>>>();
            match res {
                Ok(res) => res,
                Err(err) => return err.to_compile_error().into(),
            }
        }
        _ => panic!("Expected struct"),
    };

    let builder_fields = fields.iter().map(BuilderField::to_field);

    let builder_init_nones = fields.iter().map(BuilderField::to_init_body);
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

            pub fn build(&mut self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
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
