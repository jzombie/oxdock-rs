use proc_macro::TokenStream;
use quote::quote;
use std::path::{Path, PathBuf};
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
    match expand_embed_internal(&input) {
        Ok(ts) => ts.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

struct EmbedDslInput {
    name: Ident,
    script: LitStr,
    prebuilt: Option<LitStr>,
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

        // Optional: prebuilt: "path" or prebuilt: None
        let lookahead = input.lookahead1();
        let prebuilt = if lookahead.peek(Ident) {
            let prebuilt_label: Ident = input.parse()?;
            if prebuilt_label != "prebuilt" {
                return Err(syn::Error::new(
                    prebuilt_label.span(),
                    "expected `prebuilt` label",
                ));
            }
            input.parse::<Token![:]>()?;
            // Allow prebuilt: None as a spelled-out absence.
            if input.peek(Ident) {
                let none_ident: Ident = input.parse()?;
                if none_ident != "None" {
                    return Err(syn::Error::new(
                        none_ident.span(),
                        "expected string path or `None`",
                    ));
                }
                let _ = input.parse::<Token![,]>();
                None
            } else {
                let path: LitStr = input.parse()?;
                let _ = input.parse::<Token![,]>();
                Some(path)
            }
        } else {
            None
        };

        Ok(Self {
            name,
            script,
            prebuilt,
        })
    }
}

fn expand_embed_internal(input: &EmbedDslInput) -> syn::Result<proc_macro2::TokenStream> {
    let script_src = input.script.value();
    let span = input.script.span();

    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
        .map_err(|e| syn::Error::new(span, format!("CARGO_MANIFEST_DIR missing: {e}")))?;
    let manifest_path = Path::new(&manifest_dir);
    let is_primary = std::env::var("CARGO_PRIMARY_PACKAGE")
        .map(|v| v == "1")
        .unwrap_or(false);
    // Detect a Git repository by walking up from the consuming crate's manifest dir.
    // This supports workspace layouts where .git is at the root rather than per-crate.
    fn find_git_root(start: &Path) -> bool {
        let mut cur = Some(start);
        while let Some(p) = cur {
            if p.join(".git").exists() {
                return true;
            }
            cur = p.parent();
        }
        false
    }
    let has_git = find_git_root(manifest_path);
    let should_build = is_primary && has_git;

    let name = &input.name;

    if should_build {
        let folder = build_assets(&script_src, span)?;
        let folder_str = folder
            .to_str()
            .ok_or_else(|| syn::Error::new(span, "assets path is not valid UTF-8"))?;
        let folder_lit = syn::LitStr::new(folder_str, span);

        return Ok(quote! {
            #[derive(::rust_embed::RustEmbed)]
            #[folder = #folder_lit]
            pub struct #name;
        });
    }

    if let Some(prebuilt) = &input.prebuilt {
        let prebuilt_str = prebuilt.value();
        let prebuilt_abs = manifest_path.join(&prebuilt_str);
        if !prebuilt_abs.exists() {
            return Err(syn::Error::new(
                prebuilt.span(),
                format!("prebuilt assets folder not found: {}", prebuilt_str),
            ));
        }

        let prebuilt_lit = syn::LitStr::new(
            prebuilt_abs
                .to_str()
                .ok_or_else(|| syn::Error::new(prebuilt.span(), "prebuilt path not valid UTF-8"))?,
            prebuilt.span(),
        );
        return Ok(quote! {
            #[derive(::rust_embed::RustEmbed)]
            #[folder = #prebuilt_lit]
            pub struct #name;
        });
    }

    Err(syn::Error::new(
        span,
        "embed_dsl: refused to build assets (not primary package or .git missing) and no `prebuilt` path was provided",
    ))
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

    let final_cwd = embed_recipe_dsl::run_steps_with_context_result(&root, build_context, &steps)
        .map_err(|e| syn::Error::new(span, format!("execution error: {e}")))?;

    Ok(final_cwd)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;
    use std::env;
    use std::fs;

    #[test]
    #[serial]
    fn uses_prebuilt_when_not_primary_and_no_git() {
        let temp = tempfile::tempdir().expect("tempdir");
        let manifest_dir = temp.path();
        let prebuilt_rel = "prebuilt";
        let prebuilt_abs = manifest_dir.join(prebuilt_rel);
        fs::create_dir_all(&prebuilt_abs).expect("mkdir prebuilt");

        // Simulate crates.io tarball: no .git, not primary package.
        unsafe {
            env::remove_var("CARGO_PRIMARY_PACKAGE");
            env::remove_var("CARGO_MANIFEST_DIR");
            env::set_var("CARGO_MANIFEST_DIR", manifest_dir);
            env::set_var("CARGO_PRIMARY_PACKAGE", "0");
        }

        let input = EmbedDslInput {
            name: Ident::new("DemoAssets", proc_macro2::Span::call_site()),
            script: LitStr::new("", proc_macro2::Span::call_site()),
            prebuilt: Some(LitStr::new(prebuilt_rel, proc_macro2::Span::call_site())),
        };

        let ts = expand_embed_internal(&input).expect("prebuilt branch should succeed");
        let out = ts.to_string();
        let prebuilt_abs_str = prebuilt_abs.to_string_lossy().to_string();
        assert!(out.contains("DemoAssets"), "should define struct name");
        assert!(
            out.contains(&prebuilt_abs_str),
            "should point folder to prebuilt abs path"
        );
    }

    #[test]
    #[serial]
    fn errors_without_prebuilt_when_not_primary_and_no_git() {
        let temp = tempfile::tempdir().expect("tempdir");
        let manifest_dir = temp.path();

        unsafe {
            env::remove_var("CARGO_PRIMARY_PACKAGE");
            env::remove_var("CARGO_MANIFEST_DIR");
            env::set_var("CARGO_MANIFEST_DIR", manifest_dir);
            env::set_var("CARGO_PRIMARY_PACKAGE", "0");
        }

        let input = EmbedDslInput {
            name: Ident::new("DemoAssets", proc_macro2::Span::call_site()),
            script: LitStr::new("", proc_macro2::Span::call_site()),
            prebuilt: None,
        };

        let err = expand_embed_internal(&input).expect_err("should require prebuilt path");
        let msg = err.to_string();
        assert!(
            msg.contains("prebuilt"),
            "error should mention prebuilt path requirement"
        );
    }

    #[test]
    #[serial]
    fn builds_from_manifest_dir_when_primary_with_git() {
        let temp = tempfile::tempdir().expect("tempdir");
        let manifest_dir = temp.path();
        fs::create_dir_all(manifest_dir.join(".git")).expect("mkdir .git");

        // Source file only exists under the provided manifest dir; COPY should succeed from there.
        fs::write(manifest_dir.join("source.txt"), b"hello from manifest").expect("write source");

        unsafe {
            env::remove_var("CARGO_PRIMARY_PACKAGE");
            env::remove_var("CARGO_MANIFEST_DIR");
            env::set_var("CARGO_MANIFEST_DIR", manifest_dir);
            env::set_var("CARGO_PRIMARY_PACKAGE", "1");
        }

        let input = EmbedDslInput {
            name: Ident::new("DemoAssets", proc_macro2::Span::call_site()),
            script: LitStr::new("COPY source.txt copied.txt", proc_macro2::Span::call_site()),
            prebuilt: None,
        };

        let ts = expand_embed_internal(&input).expect("should build using manifest dir");
        let folder_path = folder_attr_path(&ts);

        let copied = std::path::Path::new(&folder_path).join("copied.txt");
        let contents = fs::read_to_string(&copied).expect("copied file readable");
        assert_eq!(
            contents, "hello from manifest",
            "copy should read from manifest dir"
        );
    }

    #[test]
    #[serial]
    fn uses_final_workdir_for_folder() {
        let temp = tempfile::tempdir().expect("tempdir");
        let manifest_dir = temp.path();
        fs::create_dir_all(manifest_dir.join(".git")).expect("mkdir .git");

        unsafe {
            env::remove_var("CARGO_PRIMARY_PACKAGE");
            env::remove_var("CARGO_MANIFEST_DIR");
            env::set_var("CARGO_MANIFEST_DIR", manifest_dir);
            env::set_var("CARGO_PRIMARY_PACKAGE", "1");
        }

        let script = [
            "MKDIR dist",
            "WRITE dist/hello.txt hi",
            "WRITE outside.txt nope",
            "WORKDIR dist",
        ]
        .join("\n");

        let input = EmbedDslInput {
            name: Ident::new("DemoAssets", proc_macro2::Span::call_site()),
            script: LitStr::new(&script, proc_macro2::Span::call_site()),
            prebuilt: None,
        };

        let ts = expand_embed_internal(&input).expect("should build using final WORKDIR");
        let folder_path = folder_attr_path(&ts);

        assert!(
            folder_path.ends_with("dist"),
            "folder should reflect final WORKDIR"
        );

        let inside = std::path::Path::new(&folder_path).join("hello.txt");
        assert!(inside.exists(), "file in final WORKDIR should exist");

        let outside = std::path::Path::new(&folder_path).join("outside.txt");
        assert!(
            !outside.exists(),
            "folder should not be the root; outside file must be absent"
        );
    }

    fn folder_attr_path(ts: &proc_macro2::TokenStream) -> String {
        let item: syn::DeriveInput = syn::parse2(ts.clone()).expect("parse derive input");
        let attr = item
            .attrs
            .iter()
            .find(|a| a.path().is_ident("folder"))
            .expect("folder attribute present");
        match attr.meta {
            syn::Meta::NameValue(ref nv) => match &nv.value {
                syn::Expr::Lit(expr_lit) => {
                    if let syn::Lit::Str(ref litstr) = expr_lit.lit {
                        return litstr.value();
                    }
                }
                _ => {}
            },
            _ => {}
        }
        panic!("folder attribute did not contain a string literal");
    }
}
