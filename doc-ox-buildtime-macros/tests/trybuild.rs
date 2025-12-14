#[test]
fn trybuild_manifest_dir() {
    use std::process::Command;

    let status = Command::new("cargo")
        .arg("run")
        .arg("--manifest-path")
        .arg("tests/fixtures/build_from_manifest/Cargo.toml")
        .arg("--quiet")
        .status()
        .expect("failed to spawn cargo");

    assert!(
        status.success(),
        "fixture build_from_manifest should compile and run"
    );
}

#[test]
fn trybuild_exit_fail() {
    use std::process::Command;

    let status = Command::new("cargo")
        .arg("run")
        .arg("--manifest-path")
        .arg("tests/fixtures/build_exit_fail/Cargo.toml")
        .arg("--quiet")
        .status()
        .expect("failed to spawn cargo");

    assert!(
        !status.success(),
        "fixture build_exit_fail should fail compilation when EXIT is nonzero"
    );
}
