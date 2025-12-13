use proc_macro::TokenStream;
use quote::quote;
use std::path::PathBuf;
use syn::parse::{Parse, ParseStream};
use syn::{Ident, LitStr, Token, parse_macro_input};

/// Macro that runs the DSL at compile time, materializes assets into a temp
/// dir, and emits a rust-embed struct pointing at that dir.
///
/// embed_dsl! {
///     name: DemoAssets,
///     script: r#"...DSL..."#,
/// }
#[proc_macro]
pub fn embed_dsl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as EmbedDslInput);
    match expand_embed(&input) {
        Ok(ts) => ts,
        Err(err) => err.to_compile_error().into(),
    }
}

struct EmbedDslInput {
    name: Ident,
    script: LitStr,
}

impl Parse for EmbedDslInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name_label: Ident = input.parse()?;
        if name_label != "name" {
            return Err(syn::Error::new(name_label.span(), "expected `name` label"));
        }
        input.parse::<Token![:]>()?;
        let name: Ident = input.parse()?;
        input.parse::<Token![,]>()?;

        let script_label: Ident = input.parse()?;
        if script_label != "script" {
            return Err(syn::Error::new(
                script_label.span(),
                "expected `script` label",
            ));
        }
        input.parse::<Token![:]>()?;
        let script: LitStr = input.parse()?;
        let _ = input.parse::<Token![,]>();

        Ok(Self { name, script })
    }
}

fn expand_embed(input: &EmbedDslInput) -> syn::Result<TokenStream> {
    let script_src = input.script.value();
    let span = input.script.span();

    let folder = build_assets(&script_src, span)?;
    let folder_str = folder
        .to_str()
        .ok_or_else(|| syn::Error::new(span, "assets path is not valid UTF-8"))?;

    let name = &input.name;
    let folder_lit = syn::LitStr::new(folder_str, span);

    Ok(quote! {
        #[derive(::rust_embed::RustEmbed)]
        #[folder = #folder_lit]
        pub struct #name;
    }
    .into())
}

fn build_assets(script: &str, span: proc_macro2::Span) -> syn::Result<PathBuf> {
    let tempdir = tempfile::Builder::new()
        .prefix("embed_recipe_")
        .tempdir()
        .map_err(|e| syn::Error::new(span, format!("failed to create temp dir: {e}")))?;
    #[allow(deprecated)]
    let root = tempdir.into_path();

    let steps = embed_recipe_dsl::parse_script(script)
        .map_err(|e| syn::Error::new(span, format!("parse error: {e}")))?;

    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
        .map_err(|e| syn::Error::new(span, format!("CARGO_MANIFEST_DIR missing: {e}")))?;
    let build_context = std::path::Path::new(&manifest_dir);

    embed_recipe_dsl::run_steps_with_context(&root, build_context, &steps)
        .map_err(|e| syn::Error::new(span, format!("execution error: {e}")))?;

    Ok(root)
}
