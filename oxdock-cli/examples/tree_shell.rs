use oxdock_cli::{parse_script, run_script};

// Embedded DSL script: builds a small demo tree and drops you into a shell inside it.
const SCRIPT: &str = r#"
# Build a demo tree and explore it interactively.
WORKDIR /
MKDIR demo/assets
MKDIR demo/logs
WRITE demo/assets/hello.txt hello
LS demo
WORKDIR demo
# To drop into the demo workspace after running the script, run this example with
# the CLI `--shell` flag (e.g. `cargo run --example tree_shell -- --shell`).
"#;

fn main() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;

    println!("temp workspace: {}", temp.path().display());
    println!("script:\n{SCRIPT}");
    println!(
        "You'll be dropped into 'demo' inside the temp workspace. Exit the shell to finish the example.\n"
    );

    let steps = parse_script(SCRIPT)?;
    run_script(temp.path(), &steps)
}
