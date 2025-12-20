#[cfg(feature = "proc-macro-api")]
mod tests {
    use oxdock_parser::script_from_braced_tokens;
    use proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

    #[test]
    fn script_from_braced_tokens_handles_paren_group() {
        let mut ts = TokenStream::new();
        ts.extend([
            TokenTree::Ident(Ident::new("RUN", Span::call_site())),
            TokenTree::Ident(Ident::new("echo", Span::call_site())),
            TokenTree::Group(Group::new(
                Delimiter::Parenthesis,
                TokenStream::from(TokenTree::Ident(Ident::new("foo", Span::call_site()))),
            )),
        ]);

        let script = script_from_braced_tokens(&ts).expect("failed to render parentheses group");
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
                TokenStream::from(TokenTree::Ident(Ident::new("BAR", Span::call_site()))),
            )),
        ]);

        let script = script_from_braced_tokens(&ts).expect("failed to render none-delimiter group");
        assert!(
            script.contains("BAR"),
            "expected none-delimiter content in script: {script}"
        );
    }

    #[test]
    fn script_from_braced_tokens_preserves_dollar_brace_interpolation() {
        let mut ts = TokenStream::new();
        ts.extend([
            TokenTree::Ident(Ident::new("ECHO", Span::call_site())),
            TokenTree::Punct(Punct::new('$', Spacing::Alone)),
            TokenTree::Group(Group::new(
                Delimiter::Brace,
                TokenStream::from(TokenTree::Ident(Ident::new("RUN", Span::call_site()))),
            )),
        ]);

        let script = script_from_braced_tokens(&ts).expect("failed to render interpolation group");
        assert!(
            script.contains("${RUN}"),
            "expected interpolation to be preserved, got: {script}"
        );
    }

    #[test]
    fn script_from_braced_tokens_terminates_capture_instruction() {
        let mut ts = TokenStream::new();
        ts.extend([
            TokenTree::Ident(Ident::new("CAPTURE", Span::call_site())),
            TokenTree::Ident(Ident::new("out_txt", Span::call_site())),
            TokenTree::Ident(Ident::new("ECHO", Span::call_site())),
            TokenTree::Ident(Ident::new("hi", Span::call_site())),
            TokenTree::Ident(Ident::new("WORKDIR", Span::call_site())),
            TokenTree::Ident(Ident::new("dist", Span::call_site())),
        ]);

        let script = script_from_braced_tokens(&ts).expect("failed to render capture instruction");
        assert!(
            script.contains("CAPTURE out_txt ECHO hi\nWORKDIR dist"),
            "expected capture to terminate before next command, got: {script}"
        );
    }
}
