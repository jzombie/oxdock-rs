use proc_macro::TokenStream;
use quote::quote;
use std::fs;
use std::fs::OpenOptions;
use std::path::{Path, PathBuf};
use syn::parse::{Parse, ParseStream};
use syn::{Ident, LitStr, Token, parse_macro_input};

/// Macro that runs the DSL at compile-time, materializes assets into a temp
/// dir, and emits a rust-embed struct pointing at that dir.
///
/// ```rust,ignore
/// use oxdock_buildtime_macros::embed;
///
/// embed! {
///     name: DemoAssets,
///     script: r#"...DSL..."#,
///     out_dir: "prebuilt",
/// }
/// ```
#[proc_macro]
pub fn embed(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as EmbedDslInput);
    match expand_embed_internal(&input) {
        Ok(ts) => ts.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

struct EmbedDslInput {
    name: Ident,
    script: ScriptSource,
    out_dir: LitStr,
}

enum ScriptSource {
    Literal(LitStr),
    Braced(proc_macro2::TokenStream),
}

impl Parse for EmbedDslInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name_label: Ident = input.parse()?;
        if name_label != "name" {
            return Err(syn::Error::new(name_label.span(), "expected `name` label"));
        }
        input.parse::<Token![:]>()?;
        let name: Ident = input.parse()?;
        let _ = input.parse::<Token![,]>().ok();

        let script_label: Ident = input.parse()?;
        if script_label != "script" {
            return Err(syn::Error::new(
                script_label.span(),
                "expected `script` label",
            ));
        }
        input.parse::<Token![:]>()?;
        let script = if input.peek(LitStr) {
            let s: LitStr = input.parse()?;
            ScriptSource::Literal(s)
        } else if input.peek(syn::token::Brace) {
            let content;
            syn::braced!(content in input);
            let ts: proc_macro2::TokenStream = content.parse()?;
            ScriptSource::Braced(ts)
        } else {
            return Err(syn::Error::new(
                input.span(),
                "expected string literal or braced script block",
            ));
        };
        let _ = input.parse::<Token![,]>().ok();

        let out_dir_label: Ident = input.parse()?;
        if out_dir_label != "out_dir" {
            return Err(syn::Error::new(
                out_dir_label.span(),
                "expected `out_dir` label",
            ));
        }
        input.parse::<Token![:]>()?;
        let out_dir: LitStr = input.parse()?;
        let _ = input.parse::<Token![,]>().ok();

        Ok(Self {
            name,
            script,
            out_dir,
        })
    }
}

fn normalize_braced_script(ts: &proc_macro2::TokenStream) -> syn::Result<String> {
    use proc_macro2::{Delimiter, TokenTree};

    fn is_command(name: &str) -> bool {
        oxdock_dsl::COMMANDS.iter().any(|c| c.as_str() == name)
    }

    fn finalize_line(lines: &mut Vec<String>, line: &mut String) {
        let trimmed = line.trim();
        if !trimmed.is_empty() {
            lines.push(trimmed.to_string());
        }
        line.clear();
    }

    fn sticky(c: char) -> bool {
        matches!(c, '/' | '.' | '-' | ':')
    }

    fn needs_space(prev: char, next: char) -> bool {
        if prev.is_whitespace() || next.is_whitespace() {
            return false;
        }
        if sticky(prev) || sticky(next) {
            return false;
        }
        if (prev == '&' && next == '&') || (prev == '|' && next == '|') {
            return false;
        }
        true
    }

    fn push_fragment(buf: &mut String, frag: &str, force_space: bool) {
        if frag.is_empty() {
            return;
        }
        let next_char = frag.chars().next().unwrap_or(' ');
        if let Some(prev) = buf.chars().rev().find(|c| !c.is_whitespace()) {
            if force_space && !prev.is_whitespace() {
                buf.push(' ');
            } else if needs_space(prev, next_char) {
                buf.push(' ');
            }
        }
        buf.push_str(frag);
    }

    fn delim_pair(delim: Delimiter) -> Option<(char, char)> {
        match delim {
            Delimiter::Parenthesis => Some(('(', ')')),
            Delimiter::Brace => Some(('{', '}')),
            Delimiter::Bracket => Some(('[', ']')),
            Delimiter::None => None,
        }
    }

    fn walk(
        ts: proc_macro2::TokenStream,
        line: &mut String,
        lines: &mut Vec<String>,
        last_was_command: &mut bool,
    ) -> syn::Result<()> {
        for tt in ts {
            match tt {
                TokenTree::Punct(ref p) if matches!(p.as_char(), ';' | ',') => {
                    finalize_line(lines, line);
                    *last_was_command = false;
                }
                TokenTree::Group(g) => {
                    if let Some((open, close)) = delim_pair(g.delimiter()) {
                        push_fragment(line, &open.to_string(), *last_was_command);
                        *last_was_command = false;
                        walk(g.stream(), line, lines, last_was_command)?;
                        push_fragment(line, &close.to_string(), *last_was_command);
                    } else {
                        walk(g.stream(), line, lines, last_was_command)?;
                    }
                }
                TokenTree::Literal(lit) => {
                    let text = syn::parse_str::<syn::LitStr>(&lit.to_string())
                        .map(|s| s.value())
                        .unwrap_or_else(|_| lit.to_string());
                    push_fragment(line, &text, *last_was_command);
                    *last_was_command = false;
                }
                TokenTree::Punct(p) => {
                    push_fragment(line, &p.as_char().to_string(), *last_was_command);
                    *last_was_command = false;
                }
                TokenTree::Ident(ident) => {
                    let ident_text = ident.to_string();
                    let is_command = is_command(&ident_text);
                    let trimmed = line.trim();
                    let guard_prefix = trimmed.starts_with('[');
                    if is_command && !trimmed.is_empty() && !guard_prefix {
                        finalize_line(lines, line);
                    }
                    push_fragment(line, &ident_text, *last_was_command);
                    *last_was_command = is_command;
                }
            }
        }
        Ok(())
    }

    let mut lines = Vec::new();
    let mut current = String::new();
    let mut last_was_command = false;
    walk(ts.clone(), &mut current, &mut lines, &mut last_was_command)?;
    finalize_line(&mut lines, &mut current);

    Ok(lines.join("\n"))
}

fn expand_embed_internal(input: &EmbedDslInput) -> syn::Result<proc_macro2::TokenStream> {
    let (script_src, span) = match &input.script {
        ScriptSource::Literal(lit) => (lit.value(), lit.span()),
        ScriptSource::Braced(ts) => (normalize_braced_script(ts)?, proc_macro2::Span::call_site()),
    };

    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
        .map_err(|e| syn::Error::new(span, format!("CARGO_MANIFEST_DIR missing: {e}")))?;
    let manifest_path = Path::new(&manifest_dir);
    let _is_primary = std::env::var("CARGO_PRIMARY_PACKAGE")
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
    // Allow building whenever a Git checkout is present. In a crates.io tarball (no .git), we
    // require the caller to supply an out_dir instead of trying to rebuild.
    let should_build = has_git;

    let name = &input.name;

    let out_dir_str = input.out_dir.value();
    let out_dir_abs = manifest_path.join(&out_dir_str);

    if should_build {
        preflight_out_dir_for_build(&out_dir_abs, input.out_dir.span())?;
        eprintln!("embed: rebuilding assets into {}", out_dir_abs.display());
        let _final_folder = build_assets(&script_src, span, &out_dir_abs)?;
        let folder_lit = syn::LitStr::new(
            out_dir_abs
                .to_str()
                .ok_or_else(|| syn::Error::new(span, "out_dir path is not valid UTF-8"))?,
            span,
        );

        return Ok(quote! {
            #[derive(::rust_embed::RustEmbed)]
            #[folder = #folder_lit]
            pub struct #name;
        });
    }

    if out_dir_abs.exists() {
        if !out_dir_abs.is_dir() {
            return Err(syn::Error::new(
                input.out_dir.span(),
                format!(
                    "out_dir exists but is not a directory: {}",
                    out_dir_abs.display()
                ),
            ));
        }
        eprintln!("embed: reusing assets at {}", out_dir_abs.display());
        let out_dir_lit = syn::LitStr::new(
            out_dir_abs.to_str().ok_or_else(|| {
                syn::Error::new(input.out_dir.span(), "out_dir path not valid UTF-8")
            })?,
            input.out_dir.span(),
        );
        return Ok(quote! {
            #[derive(::rust_embed::RustEmbed)]
            #[folder = #out_dir_lit]
            pub struct #name;
        });
    }

    Err(syn::Error::new(
        span,
        format!(
            "embed: refused to build assets (not primary package or .git missing) and out_dir missing at {}",
            out_dir_abs.display()
        ),
    ))
}

fn preflight_out_dir_for_build(out_dir: &Path, out_dir_span: proc_macro2::Span) -> syn::Result<()> {
    if out_dir.exists() {
        if !out_dir.is_dir() {
            return Err(syn::Error::new(
                out_dir_span,
                format!(
                    "out_dir exists but is not a directory: {}",
                    out_dir.display()
                ),
            ));
        }
    } else {
        fs::create_dir_all(out_dir).map_err(|e| {
            syn::Error::new(
                out_dir_span,
                format!(
                    "failed to create out_dir {} during pre-check: {e}",
                    out_dir.display()
                ),
            )
        })?;
    }

    let probe = out_dir.join(".oxdock_write_probe");
    match OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(&probe)
    {
        Ok(_) => {
            let _ = fs::remove_file(&probe);
            Ok(())
        }
        Err(e) => Err(syn::Error::new(
            out_dir_span,
            format!("out_dir not writable: {} ({e})", out_dir.display()),
        )),
    }?;

    Ok(())
}

fn build_assets(script: &str, span: proc_macro2::Span, out_dir: &Path) -> syn::Result<PathBuf> {
    // Build in a temp dir; only the final workdir gets materialized into out_dir.
    let tempdir = tempfile::Builder::new()
        .prefix("oxdock_")
        .tempdir()
        .map_err(|e| syn::Error::new(span, format!("failed to create temp dir: {e}")))?;
    #[allow(deprecated)]
    let temp_root = tempdir.into_path();

    let steps = oxdock_dsl::parse_script(script)
        .map_err(|e| syn::Error::new(span, format!("parse error: {e}")))?;

    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
        .map_err(|e| syn::Error::new(span, format!("CARGO_MANIFEST_DIR missing: {e}")))?;
    let build_context = std::path::Path::new(&manifest_dir);

    let final_cwd = oxdock_dsl::run_steps_with_context_result(&temp_root, build_context, &steps)
        .map_err(|e| syn::Error::new(span, format!("execution error: {e}")))?;

    eprintln!(
        "embed: final workdir {} (temp root {})",
        final_cwd.display(),
        temp_root.display()
    );

    let meta = fs::metadata(&final_cwd).map_err(|e| {
        syn::Error::new(
            span,
            format!(
                "final workdir missing after build: {} ({e})",
                final_cwd.display()
            ),
        )
    })?;
    if !meta.is_dir() {
        return Err(syn::Error::new(
            span,
            format!("final workdir is not a directory: {}", final_cwd.display()),
        ));
    }

    // Clean destination then copy the final workdir contents into the out_dir mount.
    if out_dir.exists() {
        clear_dir(out_dir, span)?;
    } else {
        fs::create_dir_all(out_dir).map_err(|e| {
            syn::Error::new(
                span,
                format!("failed to create out_dir {}: {e}", out_dir.display()),
            )
        })?;
    }

    copy_dir_contents(&final_cwd, out_dir, span)?;
    eprintln!(
        "embed: populated out_dir from final workdir; entries now: {}",
        count_entries(out_dir, span)?
    );

    Ok(final_cwd)
}

fn copy_dir_contents(src: &Path, dst: &Path, span: proc_macro2::Span) -> syn::Result<()> {
    for entry in fs::read_dir(src)
        .map_err(|e| syn::Error::new(span, format!("failed to read dir {}: {e}", src.display())))?
    {
        let entry = entry.map_err(|e| syn::Error::new(span, format!("dir entry error: {e}")))?;
        let file_type = entry
            .file_type()
            .map_err(|e| syn::Error::new(span, format!("file type error: {e}")))?;
        let src_path = entry.path();
        let dst_path = dst.join(entry.file_name());
        if file_type.is_dir() {
            fs::create_dir_all(&dst_path).map_err(|e| {
                syn::Error::new(
                    span,
                    format!("failed to create dir {}: {e}", dst_path.display()),
                )
            })?;
            copy_dir_contents(&src_path, &dst_path, span)?;
        } else if file_type.is_file() {
            fs::copy(&src_path, &dst_path).map_err(|e| {
                syn::Error::new(
                    span,
                    format!(
                        "failed to copy {} -> {}: {e}",
                        src_path.display(),
                        dst_path.display()
                    ),
                )
            })?;
        } else {
            return Err(syn::Error::new(
                span,
                format!("unsupported file type: {}", src_path.display()),
            ));
        }
    }
    Ok(())
}

fn clear_dir(dir: &Path, span: proc_macro2::Span) -> syn::Result<()> {
    if !dir.is_dir() {
        return Err(syn::Error::new(
            span,
            format!("out_dir exists but is not a directory: {}", dir.display()),
        ));
    }
    for entry in fs::read_dir(dir).map_err(|e| {
        syn::Error::new(
            span,
            format!("failed to read out_dir {}: {e}", dir.display()),
        )
    })? {
        let entry = entry.map_err(|e| syn::Error::new(span, format!("dir entry error: {e}")))?;
        let path = entry.path();
        let ft = entry
            .file_type()
            .map_err(|e| syn::Error::new(span, format!("file type error: {e}")))?;
        if ft.is_dir() {
            fs::remove_dir_all(&path).map_err(|e| {
                syn::Error::new(
                    span,
                    format!("failed to remove dir {}: {e}", path.display()),
                )
            })?;
        } else {
            fs::remove_file(&path).map_err(|e| {
                syn::Error::new(
                    span,
                    format!("failed to remove file {}: {e}", path.display()),
                )
            })?;
        }
    }
    Ok(())
}

fn count_entries(dir: &Path, span: proc_macro2::Span) -> syn::Result<usize> {
    let entries = fs::read_dir(dir)
        .map_err(|e| syn::Error::new(span, format!("failed to read dir {}: {e}", dir.display())))?;
    Ok(entries.count())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;
    use std::env;
    use std::fs;

    #[test]
    #[serial]
    fn errors_when_out_dir_is_file_before_build() {
        let temp = tempfile::tempdir().expect("tempdir");
        let manifest_dir = temp.path();
        fs::create_dir_all(manifest_dir.join(".git")).expect("mkdir .git");

        let assets_rel = "prebuilt";
        let assets_abs = manifest_dir.join(assets_rel);
        fs::write(&assets_abs, b"not a dir").expect("create file at out_dir path");

        unsafe {
            env::remove_var("CARGO_PRIMARY_PACKAGE");
            env::remove_var("CARGO_MANIFEST_DIR");
            env::set_var("CARGO_MANIFEST_DIR", manifest_dir);
            env::set_var("CARGO_PRIMARY_PACKAGE", "1");
        }

        let input = EmbedDslInput {
            name: Ident::new("DemoAssets", proc_macro2::Span::call_site()),
            script: ScriptSource::Literal(LitStr::new(
                "WRITE hello.txt hi",
                proc_macro2::Span::call_site(),
            )),
            out_dir: LitStr::new(assets_rel, proc_macro2::Span::call_site()),
        };

        let err = expand_embed_internal(&input).expect_err("should fail when out_dir is file");
        let msg = err.to_string();
        assert!(
            msg.contains("out_dir exists but is not a directory"),
            "message should report non-directory out_dir"
        );
    }

    #[cfg(unix)]
    #[test]
    #[serial]
    fn errors_when_out_dir_not_writable_before_build() {
        use std::os::unix::fs::PermissionsExt;

        let temp = tempfile::tempdir().expect("tempdir");
        let manifest_dir = temp.path();
        fs::create_dir_all(manifest_dir.join(".git")).expect("mkdir .git");

        let assets_rel = "prebuilt";
        let assets_abs = manifest_dir.join(assets_rel);
        fs::create_dir_all(&assets_abs).expect("mkdir out_dir");
        fs::set_permissions(&assets_abs, fs::Permissions::from_mode(0o555))
            .expect("make out_dir read-only");

        unsafe {
            env::remove_var("CARGO_PRIMARY_PACKAGE");
            env::remove_var("CARGO_MANIFEST_DIR");
            env::set_var("CARGO_MANIFEST_DIR", manifest_dir);
            env::set_var("CARGO_PRIMARY_PACKAGE", "1");
        }

        let input = EmbedDslInput {
            name: Ident::new("DemoAssets", proc_macro2::Span::call_site()),
            script: ScriptSource::Literal(LitStr::new(
                "WRITE hello.txt hi",
                proc_macro2::Span::call_site(),
            )),
            out_dir: LitStr::new(assets_rel, proc_macro2::Span::call_site()),
        };

        let err = expand_embed_internal(&input).expect_err("should fail when out_dir not writable");
        let msg = err.to_string();
        fs::set_permissions(&assets_abs, fs::Permissions::from_mode(0o755))
            .expect("restore permissions for cleanup");
        assert!(
            msg.contains("out_dir not writable"),
            "message should report non-writable out_dir"
        );
    }

    #[test]
    fn normalizes_braced_script() {
        let ts: proc_macro2::TokenStream = quote! {
            WORKDIR /
            MKDIR assets;
            WRITE assets/hello.txt "hi there";
            RUN echo && ls
        };

        let normalized = normalize_braced_script(&ts).expect("normalize braced script");
        let expected = [
            "WORKDIR /",
            "MKDIR assets",
            "WRITE assets/hello.txt hi there",
            "RUN echo && ls",
        ]
        .join("\n");

        assert_eq!(normalized, expected);
    }

    #[test]
    #[serial]
    fn uses_out_dir_when_not_primary_and_no_git() {
        let temp = tempfile::tempdir().expect("tempdir");
        let manifest_dir = temp.path();
        let assets_rel = "prebuilt";
        let assets_abs = manifest_dir.join(assets_rel);
        fs::create_dir_all(&assets_abs).expect("mkdir out_dir");

        // Simulate crates.io tarball: no .git, not primary package.
        unsafe {
            env::remove_var("CARGO_PRIMARY_PACKAGE");
            env::remove_var("CARGO_MANIFEST_DIR");
            env::set_var("CARGO_MANIFEST_DIR", manifest_dir);
            env::set_var("CARGO_PRIMARY_PACKAGE", "0");
        }

        let input = EmbedDslInput {
            name: Ident::new("DemoAssets", proc_macro2::Span::call_site()),
            script: ScriptSource::Literal(LitStr::new("", proc_macro2::Span::call_site())),
            out_dir: LitStr::new(assets_rel, proc_macro2::Span::call_site()),
        };

        let ts = expand_embed_internal(&input).expect("out_dir branch should succeed");
        let out = ts.to_string();
        assert!(out.contains("DemoAssets"), "should define struct name");

        let folder_path = folder_attr_path(&ts);
        assert_eq!(
            std::path::Path::new(&folder_path),
            assets_abs,
            "should point folder to out_dir abs path"
        );
    }

    #[test]
    #[serial]
    fn errors_without_out_dir_when_not_primary_and_no_git() {
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
            script: ScriptSource::Literal(LitStr::new("", proc_macro2::Span::call_site())),
            out_dir: LitStr::new("missing", proc_macro2::Span::call_site()),
        };

        let err = expand_embed_internal(&input).expect_err("should require out_dir path");
        let msg = err.to_string();
        assert!(
            msg.contains("out_dir missing"),
            "error should mention missing out_dir"
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
            script: ScriptSource::Literal(LitStr::new(
                "COPY source.txt copied.txt",
                proc_macro2::Span::call_site(),
            )),
            out_dir: LitStr::new("prebuilt", proc_macro2::Span::call_site()),
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
        let assets_rel = "prebuilt";

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
            script: ScriptSource::Literal(LitStr::new(&script, proc_macro2::Span::call_site())),
            out_dir: LitStr::new(assets_rel, proc_macro2::Span::call_site()),
        };

        let ts = expand_embed_internal(&input).expect("should build using final WORKDIR");
        let folder_path = folder_attr_path(&ts);
        assert!(
            folder_path.ends_with(assets_rel),
            "folder should be the out_dir path"
        );

        let inside = std::path::Path::new(&folder_path).join("hello.txt");
        assert!(
            inside.exists(),
            "file in final WORKDIR should exist in out_dir"
        );

        let outside = std::path::Path::new(&folder_path).join("outside.txt");
        assert!(
            !outside.exists(),
            "only final WORKDIR contents should be copied into out_dir"
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
