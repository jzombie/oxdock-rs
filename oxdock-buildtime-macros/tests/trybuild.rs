#[test]
fn trybuild_manifest_dir() {
    use oxdock_process::CommandBuilder;
    use std::path::Path;
    use std::fs;

    // Ensure any previously generated prebuilt* dirs are removed so tests start clean.
    let manifest = Path::new("tests/fixtures/build_from_manifest/Cargo.toml");
    if let Some(base) = manifest.parent() {
        if let Ok(entries) = fs::read_dir(base) {
            for e in entries.flatten() {
                if let Ok(ft) = e.file_type() {
                    if ft.is_dir() {
                        if let Some(name) = e.file_name().to_str() {
                            if name.starts_with("prebuilt") {
                                let _ = fs::remove_dir_all(e.path());
                            }
                        }
                    }
                }
            }
        }
    }

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
fn trybuild_exit_fail() {
    use oxdock_process::CommandBuilder;
    use std::path::Path;
    use std::fs;

    // Clean previously generated prebuilt* dirs for this fixture
    let manifest = Path::new("tests/fixtures/build_exit_fail/Cargo.toml");
    if let Some(base) = manifest.parent() {
        if let Ok(entries) = fs::read_dir(base) {
            for e in entries.flatten() {
                if let Ok(ft) = e.file_type() {
                    if ft.is_dir() {
                        if let Some(name) = e.file_name().to_str() {
                            if name.starts_with("prebuilt") {
                                let _ = fs::remove_dir_all(e.path());
                            }
                        }
                    }
                }
            }
        }
    }

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
