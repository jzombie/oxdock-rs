// TODO: Replace this ad-hoc repo-side cleanup with an isolated test harness
// (e.g. run fixtures in temporary directories) to avoid mutating the repo.

#[test]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn trybuild_manifest_dir() {
    use oxdock_process::CommandBuilder;
    // use std::path::Path;

    // Ensure any previously generated prebuilt* dirs are removed so tests start clean.
    // let manifest = Path::new("tests/fixtures/build_from_manifest/Cargo.toml");
    // clean_prebuilt_dirs(manifest);

    let mut cmd = CommandBuilder::new("cargo");
    cmd.arg("run")
        .arg("--manifest-path")
        .arg("tests/fixtures/build_from_manifest/Cargo.toml")
        .arg("--quiet");
    let status = cmd.status().expect("failed to spawn cargo");

    assert!(
        status.success(),
        "fixture build_from_manifest should compile and run"
    );
}

#[test]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn trybuild_exit_fail() {
    use oxdock_process::CommandBuilder;
    // use std::path::Path;

    // Clean previously generated prebuilt* dirs for this fixture
    // let manifest = Path::new("tests/fixtures/build_exit_fail/Cargo.toml");
    // clean_prebuilt_dirs(manifest);

    let mut cmd = CommandBuilder::new("cargo");
    cmd.arg("run")
        .arg("--manifest-path")
        .arg("tests/fixtures/build_exit_fail/Cargo.toml")
        .env("OXDOCK_EMBED_FORCE_REBUILD", "1")
        .arg("--quiet");
    let output = cmd.output().expect("failed to spawn cargo");

    assert!(
        !output.success(),
        "fixture build_exit_fail should fail compilation when EXIT is nonzero. stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}
