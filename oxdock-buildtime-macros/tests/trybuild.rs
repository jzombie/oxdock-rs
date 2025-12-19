use oxdock_fixture::FixtureBuilder;
use oxdock_fs::GuardedPath;

#[test]
#[cfg_attr(
    miri,
    ignore = "requires spawning cargo inside a copied workspace; Miri isolation forbids std::fs metadata"
)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn trybuild_manifest_dir() {
    let fixture = instantiate_fixture("build_from_manifest");

    let mut cmd = fixture.cargo();
    cmd.arg("run").arg("--quiet");
    let status = cmd.status().expect("failed to spawn cargo");

    assert!(
        status.success(),
        "fixture build_from_manifest should compile and run"
    );
}

#[test]
#[cfg_attr(
    miri,
    ignore = "requires spawning cargo inside a copied workspace; Miri isolation forbids std::fs metadata"
)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn trybuild_exit_fail() {
    let fixture = instantiate_fixture("build_exit_fail");

    let mut cmd = fixture.cargo();
    cmd.arg("run")
        .env("OXDOCK_EMBED_FORCE_REBUILD", "1")
        .arg("--quiet");
    let output = cmd.output().expect("failed to spawn cargo");

    assert!(
        !output.success(),
        "fixture build_exit_fail should fail compilation when EXIT is nonzero. stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("execution error"),
        "embed! failure should report execution error, stderr:\n{stderr}"
    );
    assert!(
        !stderr.contains("no `DemoAssets` in `demo_assets`"),
        "embed! should emit a stub so downstream code compiles, stderr:\n{stderr}"
    );
}

#[test]
#[cfg_attr(
    miri,
    ignore = "requires spawning cargo inside a copied workspace; Miri isolation forbids std::fs metadata"
)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn trybuild_run_failure_reports_cause() {
    let fixture = instantiate_fixture("run_failure_reporting");

    let mut cmd = fixture.cargo();
    cmd.arg("run")
        .env("OXDOCK_EMBED_FORCE_REBUILD", "1")
        .arg("--quiet");
    let output = cmd.output().expect("failed to spawn cargo");

    assert!(
        !output.success(),
        "fixture run_failure_reporting should fail during RUN. stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("__oxdock_missing_command__"),
        "stderr should mention the failing command, stderr:\n{stderr}"
    );
    assert!(
        stderr.contains("failed with status"),
        "stderr should include the command failure status, stderr:\n{stderr}"
    );
    assert!(
        stderr.contains("filesystem snapshot"),
        "stderr should include the filesystem snapshot context, stderr:\n{stderr}"
    );
    assert!(
        stderr.contains("(command"),
        "stderr should embed the underlying command failure inside the error chain, stderr:\n{stderr}"
    );
}

#[test]
#[cfg_attr(
    miri,
    ignore = "requires spawning cargo inside a copied workspace; Miri isolation forbids std::fs metadata"
)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn trybuild_skip_exec_rust_analyzer_env() {
    let fixture = instantiate_fixture("build_exit_fail");

    let mut cmd = fixture.cargo();
    cmd.arg("check")
        .env("RUST_ANALYZER_INTERNALS_DO_NOT_USE", "this is unstable")
        .arg("--quiet");
    let status = cmd.status().expect("failed to spawn cargo");

    assert!(
        status.success(),
        "detecting rust-analyzer env should skip execution automatically"
    );
}

#[test]
#[cfg_attr(
    miri,
    ignore = "requires spawning cargo inside a copied workspace; Miri isolation forbids std::fs metadata"
)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn trybuild_skip_exec_vscode_background() {
    let fixture = instantiate_fixture("build_exit_fail");

    let mut cmd = fixture.cargo();
    cmd.arg("check")
        .env("VSCODE_PID", "1234")
        .env_remove("TERM") // Ensure TERM is missing
        .arg("--quiet");
    let status = cmd.status().expect("failed to spawn cargo");

    assert!(
        status.success(),
        "detecting VS Code background env (VSCODE_PID set, TERM missing) should skip execution"
    );
}

#[test]
#[cfg_attr(
    miri,
    ignore = "requires spawning cargo inside a copied workspace; Miri isolation forbids std::fs metadata"
)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn trybuild_exec_vscode_integrated_terminal() {
    let fixture = instantiate_fixture("build_exit_fail");

    let mut cmd = fixture.cargo();
    cmd.arg("check")
        .env("VSCODE_PID", "1234")
        .env("TERM", "xterm-256color") // TERM is present in integrated terminal
        .arg("--quiet");
    let output = cmd.output().expect("failed to spawn cargo");

    assert!(
        !output.success(),
        "VS Code integrated terminal (VSCODE_PID set, TERM set) should NOT skip execution"
    );
}

#[test]
#[cfg_attr(
    miri,
    ignore = "requires spawning cargo inside a copied workspace; Miri isolation forbids std::fs metadata"
)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn trybuild_guard_scope() {
    let fixture = instantiate_fixture("guard_scope");

    let mut cmd = fixture.cargo();
    cmd.arg("run")
        .env("OXDOCK_EMBED_FORCE_REBUILD", "1")
        .env("TEST_SCOPE", "1")
        .arg("--quiet");
    let status = cmd.status().expect("failed to spawn cargo");

    assert!(status.success(), "guard scope fixture should succeed");
}

#[test]
#[cfg_attr(
    miri,
    ignore = "requires spawning cargo inside a copied workspace; Miri isolation forbids std::fs metadata"
)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn trybuild_no_std_embed() {
    let fixture = instantiate_fixture("no_std_embed");

    let mut cmd = fixture.cargo();
    cmd.arg("check").arg("--quiet");
    let status = cmd.status().expect("failed to spawn cargo");

    assert!(
        status.success(),
        "no_std embed fixture should compile successfully"
    );
}

#[test]
#[cfg_attr(
    miri,
    ignore = "requires spawning cargo inside a copied workspace; Miri isolation forbids std::fs metadata"
)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn trybuild_workspace_root_override() {
    let fixture = instantiate_fixture("workspace_root_override");

    let mut cmd = fixture.cargo();
    cmd.arg("run")
        .env("OXDOCK_EMBED_FORCE_REBUILD", "1")
        .arg("--quiet");
    let status = cmd.status().expect("failed to spawn cargo");

    assert!(
        status.success(),
        "workspace root override fixture should compile and run"
    );
}

fn instantiate_fixture(name: &str) -> oxdock_fixture::FixtureInstance {
    let crate_root =
        GuardedPath::new_root_from_str(env!("CARGO_MANIFEST_DIR")).expect("crate root guard");
    let fixtures_root = crate_root
        .join("tests")
        .and_then(|t| t.join("fixtures"))
        .expect("fixtures directory");
    let template = fixtures_root.join(name).expect("fixture exists");

    let workspace_path = crate_root
        .as_path()
        .parent()
        .expect("workspace root path")
        .to_path_buf();

    FixtureBuilder::new(template.as_path())
        .expect("fixture template")
        .with_path_dependency(
            "oxdock-buildtime-macros",
            workspace_path.join("oxdock-buildtime-macros"),
        )
        .with_path_dependency(
            "oxdock-fs",
            workspace_path.join("crates/internal/oxdock-fs"),
        )
        .with_path_dependency(
            "oxdock-embed",
            workspace_path.join("crates/internal/oxdock-embed"),
        )
        .instantiate()
        .expect("failed to instantiate fixture")
}
