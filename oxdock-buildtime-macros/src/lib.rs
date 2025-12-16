use oxdock_fs::{GuardedPath, PathResolver};
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{Ident, LitStr, Token, parse_macro_input};

// TODO: Update example and don't ignore
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

/// Macro similar to `embed!` but only prepares (builds/copies) the
/// out_dir at compile time and emits no runtime struct. Use this when you
/// want the assets present on disk during build but don't want a
/// `rust-embed` struct generated into the consuming crate.
#[proc_macro]
pub fn prepare(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as EmbedDslInput);
    match expand_prepare_internal(&input) {
        Ok(()) => TokenStream::new(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn expand_prepare_internal(input: &EmbedDslInput) -> syn::Result<()> {
    let (script_src, span) = match &input.script {
        ScriptSource::Literal(lit) => (lit.value(), lit.span()),
        ScriptSource::Braced(ts) => (normalize_braced_script(ts)?, proc_macro2::Span::call_site()),
    };

    let manifest_resolver =
        PathResolver::from_manifest_env().map_err(|e| syn::Error::new(span, e.to_string()))?;
    let manifest_root = manifest_resolver.root().clone();

    let is_primary = std::env::var("CARGO_PRIMARY_PACKAGE")
        .map(|v| v == "1")
        .unwrap_or(false);
    let has_git = manifest_resolver
        .has_git_dir()
        .map_err(|e| syn::Error::new(span, e.to_string()))?;

    let should_build = has_git || is_primary;

    let out_dir_str = input.out_dir.value();
    let out_dir_abs = join_guard(&manifest_root, &out_dir_str, input.out_dir.span())?;

    if should_build {
        preflight_out_dir_for_build(&out_dir_abs, input.out_dir.span())?;
        eprintln!("prepare: rebuilding assets into {}", out_dir_abs.display());
        let _final_folder = build_assets(&script_src, span, &out_dir_abs)?;
        return Ok(());
    }

    if out_dir_abs.as_path().exists() {
        if !out_dir_abs.as_path().is_dir() {
            return Err(syn::Error::new(
                input.out_dir.span(),
                format!(
                    "out_dir exists but is not a directory: {}",
                    out_dir_abs.display()
                ),
            ));
        }
        eprintln!("prepare: reusing assets at {}", out_dir_abs.display());
        return Ok(());
    }

    Err(syn::Error::new(
        span,
        format!(
            "prepare: refused to build assets (not primary package or .git missing) and out_dir missing at {}",
            out_dir_abs.display()
        ),
    ))
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

fn join_guard(base: &GuardedPath, rel: &str, span: proc_macro2::Span) -> syn::Result<GuardedPath> {
    base.join(rel)
        .map_err(|e| syn::Error::new(span, e.to_string()))
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
        oxdock_core::COMMANDS.iter().any(|c| c.as_str() == name)
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
        if let Some(prev) = buf.chars().rev().find(|c| !c.is_whitespace())
            && ((force_space && !prev.is_whitespace()) || needs_space(prev, next_char))
        {
            buf.push(' ');
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

    let manifest_resolver =
        PathResolver::from_manifest_env().map_err(|e| syn::Error::new(span, e.to_string()))?;
    let manifest_root = manifest_resolver.root().clone();

    let is_primary = std::env::var("CARGO_PRIMARY_PACKAGE")
        .map(|v| v == "1")
        .unwrap_or(false);

    let has_git = manifest_resolver
        .has_git_dir()
        .map_err(|e| syn::Error::new(span, e.to_string()))?;

    // Allow building whenever the crate is the primary package or a Git checkout is present.
    // In a crates.io tarball (no .git) or when compiling as a non-primary package, we
    // require the caller to supply an out_dir instead of trying to rebuild. Tests can force a
    // rebuild even if an out_dir already exists via OXDOCK_EMBED_FORCE_REBUILD.
    let force_rebuild = std::env::var("OXDOCK_EMBED_FORCE_REBUILD")
        .map(|v| v == "1" || v.eq_ignore_ascii_case("true"))
        .unwrap_or(false);

    let should_build = has_git || is_primary || force_rebuild;

    let name = &input.name;

    // If the filesystem is isolated (e.g. under Miri), we cannot safely access the host filesystem.
    // In this case, we skip the build process to avoid errors.
    if oxdock_fs::is_isolated() {
        eprintln!("embed: skipping build under isolated fs");
        let mod_ident = syn::Ident::new(
            &format!("__oxdock_embed_{}", name),
            proc_macro2::Span::call_site(),
        );

        // Emit a dummy struct that matches the public API but has no assets.
        // We use the oxdock_fs::define_embed macro to handle the struct definition
        // and ensure the correct cfg(miri) behavior is applied in the generated code.
        // Note: We pass a dummy folder path because the macro requires it, but it won't be used under Miri.
        return Ok(quote! {
            #[allow(clippy::disallowed_methods, clippy::disallowed_types)]
            mod #mod_ident {
                oxdock_fs::define_embed!(#name, ".");
            }
            pub use #mod_ident::#name;
        });
    }

    let out_dir_str = input.out_dir.value();
    let out_dir_abs = join_guard(&manifest_root, &out_dir_str, input.out_dir.span())?;

    if should_build {
        preflight_out_dir_for_build(&out_dir_abs, input.out_dir.span())?;
        if force_rebuild {
            eprintln!(
                "embed: force rebuilding assets into {}",
                out_dir_abs.display()
            );
        } else {
            eprintln!("embed: rebuilding assets into {}", out_dir_abs.display());
        }
        let _final_folder = build_assets(&script_src, span, &out_dir_abs)?;
        let folder_lit = syn::LitStr::new(
            out_dir_abs
                .as_path()
                .to_str()
                .ok_or_else(|| syn::Error::new(span, "out_dir path is not valid UTF-8"))?,
            span,
        );

        // Wrap the generated struct in a private module annotated with an
        // allow, then re-export it. This keeps the lint suppression scoped to
        // the generated item while avoiding call-site attributes. Tests that
        // inspect the output are updated to look through this wrapper.
        let mod_ident = syn::Ident::new(
            &format!("__oxdock_embed_{}", name),
            proc_macro2::Span::call_site(),
        );

        return Ok(quote! {
            #[allow(clippy::disallowed_methods,clippy::disallowed_types)]
            mod #mod_ident {
                oxdock_fs::define_embed!(#name, #folder_lit);
            }
            pub use #mod_ident::#name;
        });
    }

    if out_dir_abs.as_path().exists() {
        if !out_dir_abs.as_path().is_dir() {
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
            out_dir_abs.as_path().to_str().ok_or_else(|| {
                syn::Error::new(input.out_dir.span(), "out_dir path not valid UTF-8")
            })?,
            input.out_dir.span(),
        );
        let mod_ident = syn::Ident::new(
            &format!("__oxdock_embed_{}", name),
            proc_macro2::Span::call_site(),
        );

        return Ok(quote! {
            #[allow(clippy::disallowed_methods, clippy::disallowed_types)]
            mod #mod_ident {
                oxdock_fs::define_embed!(#name, #out_dir_lit);
            }
            pub use #mod_ident::#name;
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

fn preflight_out_dir_for_build(
    out_dir: &GuardedPath,
    out_dir_span: proc_macro2::Span,
) -> syn::Result<()> {
    // Build a resolver rooted at the manifest; ensure out_dir is created
    let resolver = PathResolver::from_manifest_env()
        .map_err(|e| syn::Error::new(out_dir_span, e.to_string()))?;

    // Ensure out_dir exists
    if out_dir.as_path().exists() {
        if !out_dir.as_path().is_dir() {
            return Err(syn::Error::new(
                out_dir_span,
                format!(
                    "out_dir exists but is not a directory: {}",
                    out_dir.display()
                ),
            ));
        }
    } else {
        resolver.create_dir_all_abs(out_dir).map_err(|e| {
            syn::Error::new(
                out_dir_span,
                format!(
                    "failed to create out_dir {} during pre-check: {e}",
                    out_dir.display()
                ),
            )
        })?;
    }

    // Probe writeability by writing and removing a small file through the resolver.
    let probe = out_dir
        .join(".oxdock_write_probe")
        .map_err(|e| syn::Error::new(out_dir_span, e.to_string()))?;
    match resolver.write_file(&probe, b"") {
        Ok(_) => {
            let _ = resolver.remove_file_abs(&probe);
            Ok(())
        }
        Err(e) => Err(syn::Error::new(
            out_dir_span,
            format!("out_dir not writable: {} ({e})", out_dir.display()),
        )),
    }
}

fn build_assets(
    script: &str,
    span: proc_macro2::Span,
    out_dir: &GuardedPath,
) -> syn::Result<GuardedPath> {
    // Build in a temp dir; only the final workdir gets materialized into out_dir.
    let tempdir = GuardedPath::tempdir()
        .map_err(|e| syn::Error::new(span, format!("failed to create temp dir: {e}")))?;
    let temp_root_guard = tempdir.as_guarded_path().clone();

    let steps = oxdock_core::parse_script(script)
        .map_err(|e| syn::Error::new(span, format!("parse error: {e}")))?;

    let resolver =
        PathResolver::from_manifest_env().map_err(|e| syn::Error::new(span, e.to_string()))?;
    let build_context = resolver.build_context().clone();

    let host_resolver =
        PathResolver::new_guarded(temp_root_guard.clone(), build_context.clone())
            .map_err(|e| syn::Error::new(span, format!("failed to create resolver: {e}")))?;

    let final_cwd = oxdock_core::run_steps_with_fs(Box::new(host_resolver), &steps)
        .map_err(|e| syn::Error::new(span, format!("execution error: {e}")))?;

    #[allow(clippy::disallowed_types)]
    let final_cwd_external = oxdock_fs::UnguardedPath::new(final_cwd.as_path().to_path_buf());

    eprintln!(
        "embed: final workdir {} (temp root {})",
        final_cwd.display(),
        temp_root_guard.display()
    );

    let meta = resolver
        .metadata_external(&final_cwd_external)
        .map_err(|e| {
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
    if out_dir.as_path().exists() {
        clear_dir(out_dir, span)?;
    } else {
        resolver.create_dir_all_abs(out_dir).map_err(|e| {
            syn::Error::new(
                span,
                format!("failed to create out_dir {}: {e}", out_dir.display()),
            )
        })?;
    }

    resolver
        .copy_dir_from_external(&final_cwd_external, out_dir)
        .map_err(|e| {
            syn::Error::new(
                span,
                format!("failed to copy final workdir into out_dir: {e}"),
            )
        })?;
    eprintln!(
        "embed: populated out_dir from final workdir; entries now: {}",
        count_entries(out_dir, span)?
    );

    let _ = tempdir.persist();
    Ok(final_cwd)
}

// `copy_dir_contents` replaced by `PathResolver::copy_dir_from_external`.

fn clear_dir(dir: &GuardedPath, span: proc_macro2::Span) -> syn::Result<()> {
    // Use PathResolver for deletions to keep filesystem access centralized.
    let resolver =
        PathResolver::from_manifest_env().map_err(|e| syn::Error::new(span, e.to_string()))?;

    // Validate dir is a directory (use std as this is already an existing path under manifest)
    if !dir.as_path().is_dir() {
        return Err(syn::Error::new(
            span,
            format!("out_dir exists but is not a directory: {}", dir.display()),
        ));
    }

    let entries = resolver.read_dir_entries(dir).map_err(|e| {
        syn::Error::new(
            span,
            format!("failed to read out_dir {}: {e}", dir.display()),
        )
    })?;

    for entry in entries {
        let path = entry.path();
        let guarded = GuardedPath::new(dir.root(), &path).map_err(|e| {
            syn::Error::new(
                span,
                format!("failed to guard entry {}: {e}", path.display()),
            )
        })?;
        let ft = entry
            .file_type()
            .map_err(|e| syn::Error::new(span, format!("file type error: {e}")))?;
        if ft.is_dir() {
            resolver.remove_dir_all_abs(&guarded).map_err(|e| {
                syn::Error::new(
                    span,
                    format!("failed to remove dir {}: {e}", path.display()),
                )
            })?;
        } else {
            resolver.remove_file_abs(&guarded).map_err(|e| {
                syn::Error::new(
                    span,
                    format!("failed to remove file {}: {e}", path.display()),
                )
            })?;
        }
    }
    Ok(())
}

fn count_entries(dir: &GuardedPath, span: proc_macro2::Span) -> syn::Result<usize> {
    let resolver =
        PathResolver::from_manifest_env().map_err(|e| syn::Error::new(span, e.to_string()))?;
    let entries = resolver
        .read_dir_entries(dir)
        .map_err(|e| syn::Error::new(span, format!("failed to read dir {}: {e}", dir.display())))?;
    Ok(entries.len())
}

#[cfg(test)]
#[allow(clippy::disallowed_types)]
mod tests {
    use super::*;
    #[allow(clippy::disallowed_types)]
    use oxdock_fs::{GuardedPath, UnguardedPath};
    use serial_test::serial;
    use std::env;
    use syn::parse::Parser;

    fn guard_root(path: &UnguardedPath) -> GuardedPath {
        GuardedPath::new_root(path.as_path()).unwrap()
    }

    fn resolver_for(root: &GuardedPath) -> PathResolver {
        PathResolver::new(root.as_path(), root.as_path()).unwrap()
    }

    #[test]
    #[serial]
    fn errors_when_out_dir_is_file_before_build() {
        let temp = GuardedPath::tempdir().expect("tempdir");
        let temp_root = UnguardedPath::new(temp.as_path());
        let manifest_dir = guard_root(&temp_root);
        // Create .git dir via PathResolver to centralize filesystem access.
        let resolver = resolver_for(&manifest_dir);
        resolver
            .create_dir_all_abs(&manifest_dir.join(".git").unwrap())
            .expect("mkdir .git");

        let assets_rel = "prebuilt";
        let assets_abs = manifest_dir.join(assets_rel).unwrap();
        resolver
            .write_file(&assets_abs, b"not a dir")
            .expect("create file at out_dir path");

        unsafe {
            env::remove_var("CARGO_PRIMARY_PACKAGE");
            env::remove_var("CARGO_MANIFEST_DIR");
            env::set_var("CARGO_MANIFEST_DIR", manifest_dir.as_path());
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
        let temp = GuardedPath::tempdir().expect("tempdir");
        let temp_root = UnguardedPath::new(temp.as_path());
        let manifest_dir = guard_root(&temp_root);
        let resolver = resolver_for(&manifest_dir);
        resolver
            .create_dir_all_abs(&manifest_dir.join(".git").unwrap())
            .expect("mkdir .git");

        let assets_rel = "prebuilt";
        let assets_abs = manifest_dir.join(assets_rel).unwrap();
        resolver
            .create_dir_all_abs(&assets_abs)
            .expect("mkdir out_dir");
        resolver
            .set_permissions_mode_unix(&assets_abs, 0o555)
            .expect("make out_dir read-only");

        unsafe {
            env::remove_var("CARGO_PRIMARY_PACKAGE");
            env::remove_var("CARGO_MANIFEST_DIR");
            env::set_var("CARGO_MANIFEST_DIR", manifest_dir.as_path());
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
        resolver
            .set_permissions_mode_unix(&assets_abs, 0o755)
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
        let temp = GuardedPath::tempdir().expect("tempdir");
        let temp_root = UnguardedPath::new(temp.as_path());
        let manifest_dir = guard_root(&temp_root);
        let assets_rel = "prebuilt";
        let assets_abs = manifest_dir.join(assets_rel).unwrap();
        let resolver = resolver_for(&manifest_dir);
        resolver
            .create_dir_all_abs(&assets_abs)
            .expect("mkdir out_dir");

        // Simulate crates.io tarball: no .git, not primary package.
        unsafe {
            env::remove_var("CARGO_PRIMARY_PACKAGE");
            env::remove_var("CARGO_MANIFEST_DIR");
            env::set_var("CARGO_MANIFEST_DIR", manifest_dir.as_path());
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
        let folder_guard = GuardedPath::new(
            manifest_dir.root(),
            UnguardedPath::new(&folder_path).as_path(),
        )
        .expect("guard folder path");
        assert_eq!(
            folder_guard.as_path(),
            assets_abs.as_path(),
            "should point folder to out_dir abs path"
        );
    }

    #[test]
    #[serial]
    fn prepare_errors_without_out_dir_when_not_primary_and_no_git() {
        let temp = GuardedPath::tempdir().expect("tempdir");
        let temp_root = UnguardedPath::new(temp.as_path());
        let manifest_dir = guard_root(&temp_root);

        unsafe {
            env::remove_var("CARGO_PRIMARY_PACKAGE");
            env::remove_var("CARGO_MANIFEST_DIR");
            env::set_var("CARGO_MANIFEST_DIR", manifest_dir.as_path());
            env::set_var("CARGO_PRIMARY_PACKAGE", "0");
        }

        let input = EmbedDslInput {
            name: Ident::new("DemoAssets", proc_macro2::Span::call_site()),
            script: ScriptSource::Literal(LitStr::new("", proc_macro2::Span::call_site())),
            out_dir: LitStr::new("missing", proc_macro2::Span::call_site()),
        };

        let err =
            expand_prepare_internal(&input).expect_err("prepare should require existing out_dir");
        let msg = err.to_string();
        assert!(
            msg.contains("prepare: refused to build assets") && msg.contains("missing"),
            "error should mention missing out_dir and refusal to build"
        );
    }

    #[test]
    #[serial]
    fn errors_without_out_dir_when_not_primary_and_no_git() {
        let temp = GuardedPath::tempdir().expect("tempdir");
        let temp_root = UnguardedPath::new(temp.as_path());
        let manifest_dir = guard_root(&temp_root);

        unsafe {
            env::remove_var("CARGO_PRIMARY_PACKAGE");
            env::remove_var("CARGO_MANIFEST_DIR");
            env::set_var("CARGO_MANIFEST_DIR", manifest_dir.as_path());
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
        let temp = GuardedPath::tempdir().expect("tempdir");
        let temp_root = UnguardedPath::new(temp.as_path());
        let manifest_dir = guard_root(&temp_root);
        let resolver = resolver_for(&manifest_dir);
        resolver
            .create_dir_all_abs(&manifest_dir.join(".git").unwrap())
            .expect("mkdir .git");

        // Source file only exists under the provided manifest dir; COPY should succeed from there.
        resolver
            .write_file(
                &manifest_dir.join("source.txt").unwrap(),
                b"hello from manifest",
            )
            .expect("write source");

        unsafe {
            env::remove_var("CARGO_PRIMARY_PACKAGE");
            env::remove_var("CARGO_MANIFEST_DIR");
            env::set_var("CARGO_MANIFEST_DIR", manifest_dir.as_path());
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
        let folder_guard = GuardedPath::new(
            manifest_dir.root(),
            UnguardedPath::new(&folder_path).as_path(),
        )
        .expect("guard folder path");
        let copied = folder_guard.join("copied.txt").expect("join copied.txt");
        let contents = resolver
            .read_to_string(&copied)
            .expect("copied file readable");
        assert_eq!(
            contents, "hello from manifest",
            "copy should read from manifest dir"
        );
    }

    #[test]
    #[serial]
    fn uses_final_workdir_for_folder() {
        let temp = GuardedPath::tempdir().expect("tempdir");
        let temp_root = UnguardedPath::new(temp.as_path());
        let manifest_dir = guard_root(&temp_root);
        let resolver = resolver_for(&manifest_dir);
        resolver
            .create_dir_all_abs(&manifest_dir.join(".git").unwrap())
            .expect("mkdir .git");
        let assets_rel = "prebuilt";

        unsafe {
            env::remove_var("CARGO_PRIMARY_PACKAGE");
            env::remove_var("CARGO_MANIFEST_DIR");
            env::set_var("CARGO_MANIFEST_DIR", manifest_dir.as_path());
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

        let folder_guard = GuardedPath::new(
            manifest_dir.root(),
            UnguardedPath::new(&folder_path).as_path(),
        )
        .expect("guard folder path");
        let inside = folder_guard.join("hello.txt").expect("join hello.txt");
        assert!(
            inside.as_path().exists(),
            "file in final WORKDIR should exist in out_dir"
        );

        let outside = folder_guard.join("outside.txt").expect("join outside.txt");
        assert!(
            !outside.as_path().exists(),
            "only final WORKDIR contents should be copied into out_dir"
        );
    }

    fn folder_attr_path(ts: &proc_macro2::TokenStream) -> String {
        // The macro now emits a call to oxdock_fs::define_embed!(Name, "path").
        // We need to parse this macro call and extract the second argument.
        let file: syn::File = syn::parse2(ts.clone()).expect("parse output as file");

        fn extract_folder_from_macro(m: &syn::ItemMacro) -> Option<String> {
            if m.mac.path.segments.last().unwrap().ident == "define_embed" {
                let tokens = &m.mac.tokens;
                // Parse the tokens as a comma-separated list of expressions
                let parser =
                    syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated;
                let args = parser.parse2(tokens.clone()).ok()?;

                if args.len() >= 2
                    && let syn::Expr::Lit(expr_lit) = &args[1]
                    && let syn::Lit::Str(ref litstr) = expr_lit.lit
                {
                    return Some(litstr.value());
                }
            }
            None
        }

        // Search top-level items
        for item in &file.items {
            if let syn::Item::Macro(m) = item
                && let Some(f) = extract_folder_from_macro(m)
            {
                return f;
            }
        }

        // Search inside modules
        for item in &file.items {
            if let syn::Item::Mod(m) = item
                && let Some((_, items)) = &m.content
            {
                for inner in items {
                    if let syn::Item::Macro(inner_macro) = inner
                        && let Some(f) = extract_folder_from_macro(inner_macro)
                    {
                        return f;
                    }
                }
            }
        }

        panic!("folder attribute or define_embed! macro not found");
    }
}
