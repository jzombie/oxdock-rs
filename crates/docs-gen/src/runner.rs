use anyhow::{Context, Result};
use oxdock_core::{ExecIo, run_steps_with_context_result_with_io};
use oxdock_fs::GuardedPath;
use oxdock_parser::Step;
#[allow(clippy::disallowed_types)]
use std::path::Path;

#[allow(clippy::disallowed_types)]
pub fn run(repo_root: &Path, steps: Vec<Step>, env: ExecIo) -> Result<()> {
    let root = GuardedPath::new_root(repo_root)?;
    run_steps_with_context_result_with_io(&root, &root, &steps, env)?;
    Ok(())
}

/// Execute a pure-DSL driver script.
///
/// The script text uses `$var` bindings only — runtime values enter through
/// `env` (`ExecIo::insert_inherit_env`, visible in DSL as `$name` and
/// `{{ env:NAME }}`) or `LET` bindings inside the script itself. No `#var`
/// proc-macro interpolation (compile-time only) and no manual `StepKind`
/// construction: parsing goes through the production `lower_command`
/// dispatcher via `oxdock_core::parse_script`.
#[allow(clippy::disallowed_types)]
pub fn run_script(repo_root: &Path, script: &str, env: ExecIo) -> Result<()> {
    let steps = oxdock_core::parse_script(script).context("parse docs-gen driver script")?;
    run(repo_root, steps, env)
}
