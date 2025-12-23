use oxdock_fs::{GuardedPath, normalized_path};
use std::env;
use std::io::Write;
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
use std::path::PathBuf;
use std::process::{Command, ExitStatus};

const CHILD_MODE_ENV: &str = "OXDOCK_FS_TEMP_CLEANUP_MODE";

fn maybe_run_as_child() {
    if let Ok(mode) = env::var(CHILD_MODE_ENV) {
        run_child_main(&mode);
    }
}

fn run_child_main(mode: &str) -> ! {
    let tempdir = GuardedPath::tempdir().expect("tempdir");
    println!("PATH:{}", normalized_path(tempdir.as_guarded_path()));
    let _ = std::io::stdout().flush();
    drop(tempdir);

    match mode {
        "panic" => panic!("intentional panic to test cleanup"),
        "exit1" => std::process::exit(1),
        _ => std::process::exit(0),
    }
}

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn spawn_child(mode: &str) -> (ExitStatus, Vec<PathBuf>) {
    let exe = env::current_exe().expect("current_exe");
    let output = Command::new(exe)
        .env(CHILD_MODE_ENV, mode)
        .arg("--nocapture")
        .arg("--test-threads=1")
        .output()
        .expect("child output");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let paths: Vec<PathBuf> = stdout
        .lines()
        .filter_map(|line| line.split_once("PATH:").map(|(_, rest)| rest.trim()))
        .filter(|token| !token.is_empty())
        .map(PathBuf::from)
        .collect();

    if paths.is_empty() {
        panic!(
            "child did not return a tempdir path\nstatus: {:?}\nstdout: {}\nstderr: {}",
            output.status,
            stdout,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    (output.status, paths)
}

#[cfg_attr(
    miri,
    ignore = "GuardedPath::tempdir relies on OS tempdirs; blocked under Miri isolation"
)]
#[test]
fn tempdir_dropped_on_successful_exit() {
    maybe_run_as_child();
    let (status, paths) = spawn_child("ok");
    assert!(status.success(), "child should exit successfully: {status}");
    assert!(paths.iter().all(|path| !path.exists()));
}

#[cfg_attr(
    miri,
    ignore = "GuardedPath::tempdir relies on OS tempdirs; blocked under Miri isolation"
)]
#[test]
fn tempdir_dropped_on_panic_exit() {
    maybe_run_as_child();
    let (status, paths) = spawn_child("panic");
    assert!(!status.success(), "panic should fail: {status}");
    assert!(paths.iter().all(|path| !path.exists()));
}

#[cfg_attr(
    miri,
    ignore = "GuardedPath::tempdir relies on OS tempdirs; blocked under Miri isolation"
)]
#[test]
fn tempdir_dropped_before_error_exit() {
    maybe_run_as_child();
    let (status, paths) = spawn_child("exit1");
    assert!(
        !status.success(),
        "child should exit with failure code: {status}"
    );
    assert!(paths.iter().all(|path| !path.exists()));
}

#[cfg_attr(
    miri,
    ignore = "GuardedPath::tempdir relies on OS tempdirs; blocked under Miri isolation"
)]
#[test]
fn cleanup_skips_live_tempdir() {
    let temp = GuardedPath::tempdir().expect("tempdir");
    let path = temp.as_guarded_path().to_path_buf();
    assert!(path.exists());

    GuardedPath::cleanup_stale_tempdirs().expect("cleanup");
    assert!(path.exists());

    drop(temp);
    assert!(!path.exists());
}
