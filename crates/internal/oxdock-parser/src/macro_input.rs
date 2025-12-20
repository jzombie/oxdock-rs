//! Helpers that let proc-macro inputs reuse the regular string parser.
//!
//! The macros ultimately want everything to flow through `parse_script`, since
//! that code already does the heavy lifting of guard handling, scope tracking,
//! and AST construction.  Unfortunately `TokenStream` values do not retain
//! whitespace or “line” structure, so we first have to rebuild a textual DSL
//! representation that the parser understands.  The `sticky`/`needs_space`
//! helpers below exist solely to recreate enough spacing for commands such as
//! `ENV FOO=bar` or `RUN echo && ls` to look exactly like the string DSL,
//! keeping both pathways unified.

use super::{Command, Step, parse_script};
use anyhow::Result;
use proc_macro2::{Delimiter, TokenStream as TokenStream2, TokenTree};
use syn::parse::{Parse, ParseStream};
use syn::{Ident, LitStr, Token};

/// Parsed macro arguments for `embed!` and `prepare!`.
pub struct DslMacroInput {
    pub name: Ident,
    pub script: ScriptSource,
    pub out_dir: LitStr,
}

/// The script payload, either as a literal string or a braced token stream.
pub enum ScriptSource {
    Literal(LitStr),
    Braced(TokenStream2),
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
            let ts: TokenStream2 = content.parse()?;
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

fn finalize_line(lines: &mut Vec<String>, line: &mut String) {
    let trimmed = line.trim();
    if !trimmed.is_empty() {
        lines.push(trimmed.to_string());
    }
    line.clear();
}

fn sticky(c: char) -> bool {
    matches!(c, '/' | '.' | '-' | ':' | '=' | '$' | '{' | '}')
}

fn needs_space(prev: char, next: char) -> bool {
    if next == ';' {
        return false;
    }
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

fn current_line_command(line: &str) -> Option<Command> {
    let trimmed = line.trim_start();
    let head = trimmed.split_whitespace().next()?;
    Command::parse(head)
}

fn walk(
    ts: TokenStream2,
    line: &mut String,
    lines: &mut Vec<String>,
    last_was_command: &mut bool,
    in_interpolation: bool,
) -> Result<()> {
    for tt in ts {
        match tt {
            TokenTree::Group(g) => {
                if let Some((open, close)) = delim_pair(g.delimiter()) {
                    match g.delimiter() {
                        Delimiter::Brace => {
                            if line.trim_end().ends_with('$') {
                                push_fragment(line, &open.to_string(), false);
                                *last_was_command = false;
                                walk(g.stream(), line, lines, last_was_command, true)?;
                                push_fragment(line, &close.to_string(), false);
                            } else {
                                finalize_line(lines, line);
                                line.push(open);
                                finalize_line(lines, line);
                                *last_was_command = false;
                                walk(g.stream(), line, lines, last_was_command, false)?;
                                finalize_line(lines, line);
                                line.push(close);
                                finalize_line(lines, line);
                                *last_was_command = false;
                            }
                        }
                        Delimiter::Bracket => {
                            finalize_line(lines, line);
                            push_fragment(line, &open.to_string(), false);
                            finalize_line(lines, line);
                            walk(g.stream(), line, lines, last_was_command, false)?;
                            finalize_line(lines, line);
                            push_fragment(line, &close.to_string(), false);
                            finalize_line(lines, line);
                        }
                        _ => {
                            push_fragment(line, &open.to_string(), *last_was_command);
                            *last_was_command = false;
                            walk(g.stream(), line, lines, last_was_command, in_interpolation)?;
                            push_fragment(line, &close.to_string(), *last_was_command);
                        }
                    }
                } else {
                    walk(g.stream(), line, lines, last_was_command, in_interpolation)?;
                }
            }
            TokenTree::Literal(lit) => {
                push_fragment(line, &lit.to_string(), *last_was_command);
                *last_was_command = false;
            }
            TokenTree::Punct(p) => {
                let ch = p.as_char();
                let force_space = *last_was_command && ch != ';';
                push_fragment(line, &ch.to_string(), force_space);
                *last_was_command = false;
            }
            TokenTree::Ident(ident) => {
                let ident_text = ident.to_string();
                if in_interpolation {
                    push_fragment(line, &ident_text, false);
                    *last_was_command = false;
                    continue;
                }
                let is_command = super::Command::parse(&ident_text).is_some();
                let trimmed = line.trim();
                let guard_prefix = trimmed.starts_with('[');
                let mut should_finalize = false;
                if is_command && !trimmed.is_empty() && !guard_prefix {
                    should_finalize =
                        !matches!(current_line_command(trimmed), Some(Command::Capture));
                }
                if should_finalize {
                    finalize_line(lines, line);
                }
                push_fragment(line, &ident_text, *last_was_command);
                *last_was_command = is_command;
            }
        }
    }
    Ok(())
}

/// Convert a braced Rust token stream into textual DSL lines.
pub fn script_from_braced_tokens(ts: &TokenStream2) -> Result<String> {
    let mut lines = Vec::new();
    let mut current = String::new();
    let mut last_was_command = false;
    walk(
        ts.clone(),
        &mut current,
        &mut lines,
        &mut last_was_command,
        false,
    )?;
    finalize_line(&mut lines, &mut current);
    Ok(lines.join("\n"))
}

/// Parse a braced token stream directly into DSL steps.
pub fn parse_braced_tokens(ts: &TokenStream2) -> Result<Vec<Step>> {
    let script = script_from_braced_tokens(ts)?;
    parse_script(&script)
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_str;

    #[test]
    fn parse_dsl_macro_input_literal_script() {
        let input: DslMacroInput =
            parse_str("name: foo, script: \"RUN echo hi\", out_dir: \"target/out\"")
                .expect("parse literal script");
        assert!(matches!(input.script, ScriptSource::Literal(_)));
        assert_eq!(input.name.to_string(), "foo");
        assert_eq!(input.out_dir.value(), "target/out");
    }

    #[test]
    fn parse_dsl_macro_input_braced_script() {
        let input: DslMacroInput =
            parse_str("name: foo, script: { RUN echo hi }, out_dir: \"out\"")
                .expect("parse braced script");
        assert!(matches!(input.script, ScriptSource::Braced(_)));
    }

    #[test]
    fn parse_dsl_macro_input_rejects_unknown_label() {
        let err =
            parse_str::<DslMacroInput>("names: foo, script: \"RUN echo hi\", out_dir: \"out\"")
                .err()
                .expect("unknown label should fail");
        assert!(
            err.to_string().contains("expected `name` label"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn parse_dsl_macro_input_rejects_invalid_script_label() {
        let err =
            parse_str::<DslMacroInput>("name: foo, scripts: \"RUN echo hi\", out_dir: \"out\"")
                .err()
                .expect("invalid script label should fail");
        assert!(
            err.to_string().contains("expected `script` label"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn parse_dsl_macro_input_rejects_invalid_out_dir_label() {
        let err =
            parse_str::<DslMacroInput>("name: foo, script: \"RUN echo hi\", outdirs: \"out\"")
                .err()
                .expect("invalid out_dir label should fail");
        assert!(
            err.to_string().contains("expected `out_dir` label"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn parse_dsl_macro_input_rejects_invalid_script() {
        let err = parse_str::<DslMacroInput>("name: foo, script: bar, out_dir: \"out\"")
            .err()
            .expect("invalid script should fail");
        assert!(
            err.to_string()
                .contains("expected string literal or braced script block"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn finalize_line_skips_empty_lines() {
        let mut lines = Vec::new();
        let mut buf = String::new();
        finalize_line(&mut lines, &mut buf);
        assert!(lines.is_empty());
        buf.push_str("  RUN echo hi  ");
        finalize_line(&mut lines, &mut buf);
        assert_eq!(lines, vec!["RUN echo hi"]);
    }

    #[test]
    fn needs_space_filters_tokens() {
        assert!(!needs_space('a', ';'));
        assert!(!needs_space('a', ' '));
        assert!(!needs_space(' ', 'a'));
        assert!(!needs_space('/', 'a'));
        assert!(!needs_space('a', '/'));
        assert!(!needs_space('&', '&'));
        assert!(!needs_space('|', '|'));
        assert!(needs_space('a', 'b'));
    }

    #[test]
    fn push_fragment_respects_empty_and_spacing() {
        let mut buf = String::new();
        push_fragment(&mut buf, "", false);
        assert!(buf.is_empty());
        push_fragment(&mut buf, "RUN", false);
        push_fragment(&mut buf, "echo", true);
        assert_eq!(buf, "RUN echo");
    }
}
