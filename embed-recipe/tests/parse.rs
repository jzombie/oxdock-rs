use embed_recipe::{parse_script, Options, ScriptSource, Step};

#[test]
fn parse_basic_script() {
    let script = r#"
# build and publish
WORKDIR client
RUN npm run build
WORKDIR /
COPY client/dist client/dist
SYMLINK server/dist ../client/dist
RUN cargo publish --workspace --locked
MKDIR tmp
LS tmp
WRITE tmp/file.txt hello
"#;

    let steps = parse_script(script).expect("parse should succeed");
    assert_eq!(steps.len(), 9);
    if let Step::Workdir(ref p) = steps[0] {
        assert_eq!(p, "client");
    } else {
        panic!();
    }
    if let Step::Run(ref p) = steps[1] {
        assert_eq!(p, "npm run build");
    } else {
        panic!();
    }
}

#[test]
fn parse_shell_step() {
    let script = "SHELL";
    let steps = parse_script(script).expect("parse should succeed");
    assert_eq!(steps.len(), 1);
    if let Step::Shell = steps[0] {
    } else {
        panic!();
    }
}

#[test]
fn parse_mkdir_ls_write() {
    let script = "MKDIR a/b\nLS a\nWRITE a/b/file.txt hi";
    let steps = parse_script(script).expect("parse should succeed");
    assert_eq!(steps.len(), 3);
    if let Step::Mkdir(ref p) = steps[0] {
        assert_eq!(p, "a/b");
    } else {
        panic!();
    }
}

#[test]
fn parse_options_accepts_stdin_dash() {
    let mut args = "--script -".split_whitespace().map(String::from);
    let opts = Options::parse(&mut args).expect("options parse should succeed");
    match opts.script {
        ScriptSource::Stdin => {}
        _ => panic!("expected stdin source when passing '-'"),
    }
}

#[test]
fn parse_options_defaults_to_stdin() {
    let mut args = std::iter::empty();
    let opts = Options::parse(&mut args).expect("options parse should succeed");
    match opts.script {
        ScriptSource::Stdin => {}
        _ => panic!("expected stdin source by default"),
    }
}
