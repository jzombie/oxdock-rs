//! Rust-level parsing and normalization of the `embed!`/`prepare!` arguments.
//!
//! The DSL itself lives in `oxdock-parser`, but proc macros first receive raw
//! Rust tokens. We have to peel off the macro arguments (name/script/out_dir)
//! with `syn`, and when the user supplies `script: { ... }` we normalize those
//! Rust tokens back into the textual DSL before handing everything to
//! `oxdock_core::parse_script`.

use syn::parse::{Parse, ParseStream};
use syn::{Ident, LitStr, Token};

pub(crate) struct DslMacroInput {
    pub(crate) name: Ident,
    pub(crate) script: ScriptSource,
    pub(crate) out_dir: LitStr,
}

pub(crate) enum ScriptSource {
    Literal(LitStr),
    Braced(proc_macro2::TokenStream),
}

impl Parse for DslMacroInput {
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

/// Convert a braced Rust token stream into textual DSL lines.
///
/// `syn` gives us Rust tokens, not the OxDock DSL. This function walks the
/// tokens that appeared inside `script: { ... }`, recreates the textual DSL,
/// and keeps guard/brace hints intact so the canonical parser can run.
pub(crate) fn normalize_braced_script(ts: &proc_macro2::TokenStream) -> syn::Result<String> {
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
                        match g.delimiter() {
                            Delimiter::Brace => {
                                finalize_line(lines, line);
                                line.push(open);
                                finalize_line(lines, line);
                                *last_was_command = false;
                                walk(g.stream(), line, lines, last_was_command)?;
                                finalize_line(lines, line);
                                line.push(close);
                                finalize_line(lines, line);
                                *last_was_command = false;
                            }
                            Delimiter::Bracket => {
                                finalize_line(lines, line);
                                push_fragment(line, &open.to_string(), false);
                                *last_was_command = false;
                                walk(g.stream(), line, lines, last_was_command)?;
                                push_fragment(line, &close.to_string(), false);
                            }
                            _ => {
                                push_fragment(line, &open.to_string(), *last_was_command);
                                *last_was_command = false;
                                walk(g.stream(), line, lines, last_was_command)?;
                                push_fragment(line, &close.to_string(), *last_was_command);
                            }
                        }
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
