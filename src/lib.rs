use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, DeriveInput, Type};


/// Builder derive 宏
/// ```rust
/// use builder_derive::Builder;
///
/// #[derive(Builder)]
/// struct Command {
///     executable: String,
///     #[builder(each = "arg")]
///     args: Vec<String>,
///     env: Option<Vec<String>>,
///     current_dir: Option<String>,
/// }
///
/// let mut builder: CommandBuilder = Command::builder();
/// builder.executable("cargo".to_string());
/// builder.env(vec!["PATH=/home/rookie/work".to_string()]);
/// // 相比于 env 一次性传入所有参数，使用 #[builder(each = "xx")]
/// // 可以每次push一个元素，多次调用
/// builder.arg("run".to_string());
/// builder.arg("--release".to_string());
/// builder.current_dir("/home/rookie/space".to_string());
/// let command = builder.build();
/// ```
#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    // 把 TokenStream 解析为语法树
    let input = parse_macro_input!(input as DeriveInput);
    // indent 即为被修饰结构体名称相关数据结构
    let name = input.ident;
    let bname = format!("{}Builder", name);
    // 构建一个新的 indent
    let bident = syn::Ident::new(&bname, name.span());

    let named = match input.data {
        syn::Data::Struct(s_data) => match s_data.fields {
            syn::Fields::Named(named) => named.named,
            _ => panic!("only support named field"),
        },
        _ => panic!("only support struct"),
    };

    // builder 字段
    let bfields = named.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if is_wrapper_type("Option", ty) {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    // builder 方法
    let b_methods = named.iter().map(|f| {
        let name = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        let mut field_meta = FieldMeta { each: None };
        f.attrs.iter().for_each(|attr| {
            if let Some(first) = attr.path.segments.first() {
                if first.ident == "builder" {
                    let meta = attr
                        .parse_meta()
                        .expect(&format!("failed to parse {} meta", name));
                    &mut field_meta.update(f, &meta);
                }
            }
        });
        match (is_wrapper_type("Option", ty), &field_meta.each) {
            (true, Some(builder)) => {
                let inner_ty = take_inner_type(ty);
                if !is_wrapper_type("Vec", &inner_ty) {
                    panic!("`each` attr is only allow on Vec type");
                }
                let inner_ty = take_inner_type(inner_ty);
                let builder = syn::Ident::new(builder, f.span());
                quote! {
                    pub fn #builder(&mut self, #builder: #inner_ty) {
                        if let Some(filed) = self.#name.as_mut() {
                            field.push(#builder)
                        } else {
                            self.#name = std::option::Option::Some(vec![#builder])
                        }
                    }
                }
            }
            (true, None) => {
                let inner_ty = take_inner_type(ty);
                quote! {
                    pub fn #name(&mut self, #name: #inner_ty) {
                        self.#name =  std::option::Option::Some(#name);
                    }
                }
            }
            (false, Some(builder)) => {
                if !is_wrapper_type("Vec", ty) {
                    panic!("`each` attr is only allow on Vec type");
                }
                let inner_ty = take_inner_type(ty);
                let builder = syn::Ident::new(builder, f.span());
                quote! {
                    pub fn #builder(&mut self, #builder: #inner_ty) {
                        if let Some(field) = self.#name.as_mut() {
                            field.push(#builder)
                        } else {
                            self.#name = std::option::Option::Some(vec![#builder])
                        }
                    }
                }
            }
            (false, None) => {
                quote! {
                    pub fn #name(&mut self, #name: #ty) {
                        self.#name = std::option::Option::Some(#name);
                    }
                }
            }
        }
    });

    // 调用 builder.build() 时的方法
    let build_fields = named.iter().map(|f| {
        let name = &f.ident;
        if is_wrapper_type("Option", &f.ty) {
            quote! { #name: self.#name.clone() }
        } else {
            quote! { #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))? }
        }
    });

    // 最后填充扩展 AST
    let expanded = quote! {
        impl #name {
            pub fn builder() -> #bident {
                #bident::default()
            }
        }

        #[derive(std::default::Default)]
        pub struct #bident {
            // 以“,”为分隔符展开一个可迭代对象
            // ref: https://docs.rs/quote/1.0.7/quote/macro.quote.html#interpolation
            #(#bfields,)*
        }

        impl #bident {
            #(#b_methods)*

            pub fn build(&self) ->  std::result::Result<#name, std::boxed::Box<dyn std::error::Error>>  {
                std::result::Result::Ok(#name {
                    #(#build_fields,)*
                })
            }
        }

    };

    //把展开后的 AST 返回给编译器
    TokenStream::from(expanded)
}

fn is_wrapper_type(wrapper: &str, ty: &syn::Type) -> bool {
    match ty {
        Type::Path(path) => path.path.segments.first().unwrap().ident.to_string() == wrapper,
        _ => false,
    }
}

fn take_inner_type(ty: &syn::Type) -> &syn::Type {
    if let syn::Type::Path(ref p) = ty {
        let opt_ty = p.path.segments.first().unwrap();
        match &opt_ty.arguments {
            syn::PathArguments::AngleBracketed(args) => match args.args.first().unwrap() {
                syn::GenericArgument::Type(t) => t,
                _ => panic!("can't infer type argument{}", stringify!(t)),
            },
            _ => panic!("can't infer type from {}", stringify!(ty)),
        }
    } else {
        panic!("not support")
    }
}

#[derive(Debug)]
struct FieldMeta {
    each: Option<String>,
}

impl FieldMeta {
    fn update(&mut self, field: &syn::Field, meta: &syn::Meta) {
        match meta {
            syn::Meta::List(meta_list) => meta_list.nested.iter().for_each(|m| match m {
                syn::NestedMeta::Meta(meta) => match meta {
                    syn::Meta::Path(_) => {}
                    syn::Meta::List(_) => {}
                    syn::Meta::NameValue(kv) => {
                        let key = &kv.path.segments.first().unwrap().ident;
                        if let syn::Lit::Str(lit) = &kv.lit {
                            if key == "each" {
                                self.each = Some(lit.value());
                            }
                        } else {
                            panic!(
                                "{} attr {} expect string value",
                                field.ident.as_ref().unwrap(),
                                key
                            );
                        }
                    }
                },
                syn::NestedMeta::Lit(_) => {}
            }),
            _ => {
                panic!(
                    "only support `#[builder(..)]` format attribute {} {:?}",
                    field.ident.as_ref().unwrap(),
                    meta.span()
                );
            }
        };
    }
}
