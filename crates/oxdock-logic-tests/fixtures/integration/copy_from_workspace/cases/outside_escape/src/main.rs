use oxdock_core::{run_steps_with_context_result_with_io, ExecIo};
use oxdock_fs::{GuardedPath, PathResolver};
use oxdock_parser::parse_script;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let tempdir = GuardedPath::tempdir()?;
    let snapshot_root = tempdir.as_guarded_path().clone();
    let workspace_root = tempdir.as_guarded_path().clone();

    let resolver = PathResolver::new_guarded(snapshot_root.clone(), workspace_root.clone())?;

    // Attempt to copy an absolute path outside the workspace
    let outside = "/etc/passwd"; // likely outside the guarded workspace in tests
    let script = format!("COPY --from-current-workspace {outside} out/target_escape", outside = outside);
    let steps = parse_script(&script)?;

    // This should fail
    let res = run_steps_with_context_result_with_io(&snapshot_root, &workspace_root, &steps, ExecIo::new());
    assert!(res.is_err(), "expected COPY from outside workspace to fail");
    Ok(())
}
