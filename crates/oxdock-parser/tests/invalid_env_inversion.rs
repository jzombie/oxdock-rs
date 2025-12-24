use oxdock_parser::parse_script;

#[test]
fn disallow_bang_env_equals_syntax() {
    let script = "[!env:A==1] RUN echo no";
    parse_script(script).expect_err("!env:A==1 should be rejected by the parser");
}
