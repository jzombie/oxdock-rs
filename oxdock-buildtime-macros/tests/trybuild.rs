// TODO: A better fixture should be used instead of this, especially one that auto-cleans
// up after itself. A previous version did try to clean up after itself but was disabled
// due to flakiness. One possibility is to use:  https://crates.io/crates/trybuild This
// was originally experimented with but we wound up reverting to a manual approach.

use oxdock_fixture::FixtureBuilder;
use oxdock_fs::GuardedPath;

#[test]
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
        .with_version_dependency("rust-embed", "8.9.0")
        .instantiate()
        .expect("failed to instantiate fixture")
}
