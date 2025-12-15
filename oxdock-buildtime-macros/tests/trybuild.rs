#[cfg_attr(miri, ignore)]
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

#[cfg_attr(miri, ignore)]
#[test]
fn trybuild_exit_fail() {
    use std::process::Command;

    let output = Command::new("cargo")
        .arg("run")
        .arg("--manifest-path")
        .arg("tests/fixtures/build_exit_fail/Cargo.toml")
        .env("OXDOCK_EMBED_FORCE_REBUILD", "1")
        .arg("--quiet")
        .output()
        .expect("failed to spawn cargo");

    assert!(
        !output.status.success(),
        "fixture build_exit_fail should fail compilation when EXIT is nonzero. stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}
