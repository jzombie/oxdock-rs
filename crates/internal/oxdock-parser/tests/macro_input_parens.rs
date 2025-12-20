#[cfg(feature = "proc-macro-api")]
mod tests {
    use oxdock_parser::script_from_braced_tokens;
    use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};

    #[test]
    fn script_from_braced_tokens_handles_paren_group() {
        let mut ts = TokenStream::new();
        ts.extend([
            TokenTree::Ident(Ident::new("RUN", Span::call_site())),
            TokenTree::Ident(Ident::new("echo", Span::call_site())),
            TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                TokenStream::from(TokenTree::Ident(Ident::new(
                    "foo",
                    Span::call_site(),
                ))),
            )),
        ]);

        let script =
            script_from_braced_tokens(&ts).expect("failed to render parentheses group");
        assert!(
            script.contains("( foo )"),
            "expected parentheses group in script: {script}"
        );
    }

    #[test]
    fn script_from_braced_tokens_handles_none_group() {
        let mut ts = TokenStream::new();
        ts.extend([
            TokenTree::Ident(Ident::new("RUN", Span::call_site())),
            TokenTree::Ident(Ident::new("echo", Span::call_site())),
            TokenTree::Group(Group::new(
                Delimiter::None,
                TokenStream::from(TokenTree::Ident(Ident::new(
                    "BAR",
                    Span::call_site(),
                ))),
            )),
        ]);

        let script =
            script_from_braced_tokens(&ts).expect("failed to render none-delimiter group");
        assert!(
            script.contains("BAR"),
            "expected none-delimiter content in script: {script}"
        );
    }
}
