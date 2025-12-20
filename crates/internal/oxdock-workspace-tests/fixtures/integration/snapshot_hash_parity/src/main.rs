use oxdock_buildtime_macros::{embed, prepare};
use oxdock_cli::{ExecutionResult, Options, ScriptSource, execute_with_result};
use oxdock_core::run_steps_with_context_result;
use oxdock_fs::{GuardedPath, PathResolver};
use oxdock_parser::parse_script;
use std::error::Error;

embed! {
    name: SnapshotAssets,
    script: {
        MKDIR data/inner
        WRITE data/inner/a.txt alpha
        WRITE data/b.txt beta
        CAPTURE hash.txt HASH_SHA256 data
    },
    out_dir: "prebuilt",
}

prepare! {
    name: SnapshotPrepared,
    script: {
        MKDIR data/inner
        WRITE data/inner/a.txt alpha
        WRITE data/b.txt beta
        CAPTURE hash.txt HASH_SHA256 data
    },
    out_dir: "prebuilt_prepare",
}

const SCRIPT: &str = r#"
    MKDIR data/inner
    WRITE data/inner/a.txt alpha
    WRITE data/b.txt beta
    CAPTURE hash.txt HASH_SHA256 data
"#;

fn ensure(condition: bool, message: &str) -> Result<(), Box<dyn Error>> {
    if condition {
        Ok(())
    } else {
        Err(message.into())
    }
}

fn read_hash(resolver: &PathResolver, root: &GuardedPath) -> Result<String, Box<dyn Error>> {
    let hash_path = root.join("hash.txt")?;
    let contents = resolver.read_to_string(&hash_path)?;
    Ok(contents.trim().to_string())
}

fn read_manifest_hash(
    resolver: &PathResolver,
    rel_path: &str,
) -> Result<String, Box<dyn Error>> {
    let hash_path = resolver.root().join(rel_path)?;
    let contents = resolver.read_to_string(&hash_path)?;
    Ok(contents.trim().to_string())
}

fn main() -> Result<(), Box<dyn Error>> {
    let workspace = GuardedPath::tempdir()?;
    let workspace_root = workspace.as_guarded_path().clone();
    let resolver = PathResolver::new_guarded(workspace_root.clone(), workspace_root.clone())?;

    let script_path = workspace_root.join("script.ox")?;
    resolver.write_file(&script_path, SCRIPT.as_bytes())?;

    let opts = Options {
        script: ScriptSource::Path(script_path),
        shell: false,
    };

    let ExecutionResult { tempdir, final_cwd } = execute_with_result(opts, workspace_root.clone())?;
    let cli_resolver = PathResolver::new_guarded(
        tempdir.as_guarded_path().clone(),
        workspace_root.clone(),
    )?;
    let cli_hash = read_hash(&cli_resolver, &final_cwd)?;

    let core_temp = GuardedPath::tempdir()?;
    let core_root = core_temp.as_guarded_path().clone();
    let core_resolver = PathResolver::new_guarded(core_root.clone(), workspace_root.clone())?;
    let steps = parse_script(SCRIPT)?;
    let core_final = run_steps_with_context_result(&core_root, &workspace_root, &steps)?;
    let core_hash = read_hash(&core_resolver, &core_final)?;

    let embedded = SnapshotAssets::get("hash.txt").ok_or("missing hash.txt")?;
    let embedded_hash = std::str::from_utf8(embedded.data.as_ref())?
        .trim()
        .to_string();

    ensure(cli_hash == core_hash, "CLI/core hash mismatch")?;
    ensure(cli_hash == embedded_hash, "CLI/embed hash mismatch")?;

    let manifest_resolver = PathResolver::from_manifest_env()?;
    let prepared_hash = read_manifest_hash(&manifest_resolver, "prebuilt_prepare/hash.txt")?;
    ensure(cli_hash == prepared_hash, "CLI/prepare hash mismatch")?;

    Ok(())
}
