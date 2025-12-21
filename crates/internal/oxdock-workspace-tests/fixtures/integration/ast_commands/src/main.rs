use anyhow::{Context, Result, anyhow};
use oxdock_core::run_steps_with_context;
use oxdock_fs::{
    GuardedPath, GuardedTempDir, PathResolver, discover_workspace_root, ensure_git_identity,
    is_isolated,
};
use oxdock_parser::{Step, StepKind};
use oxdock_process::CommandBuilder;
use sha2::{Digest, Sha256};
use std::collections::{BTreeSet, HashMap, HashSet};
use toml_edit::DocumentMut;

type SetupFn = fn(&GuardedPath, &GuardedPath) -> Result<()>;
type VerifyFn = fn(&GuardedPath, &GuardedPath) -> Result<()>;

#[derive(Clone, Copy)]
enum BuildContext {
    Local,
    LocalSubdir(&'static str),
    Snapshot,
}

struct ScriptCase {
    build_context: BuildContext,
    setup: SetupFn,
    verify: VerifyFn,
    expect_error: Option<&'static str>,
}

struct CoverageSpec {
    coverage: HashMap<String, Vec<String>>,
    extras: Vec<String>,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("fixture failed: {err:#}");
        std::process::exit(1);
    }
}

fn run() -> Result<()> {
    let resolver = PathResolver::from_manifest_env().context("resolve fixture manifest dir")?;

    let coverage = load_coverage(&resolver)?;
    let scripts = collect_scripts(&coverage)?;
    let script_data = load_scripts(&resolver, &scripts)?;
    let cases = script_cases();

    assert_coverage(&script_data, &coverage).context("validate AST command coverage")?;

    for script in scripts {
        let case = cases
            .get(script.as_str())
            .ok_or_else(|| anyhow!("missing ScriptCase for {}", script))?;
        let steps = script_data
            .get(script.as_str())
            .ok_or_else(|| anyhow!("missing parsed steps for {}", script))?;
        let script_path = resolver.root().join(&script)?;
        run_case(case, steps, &script_path)
            .with_context(|| format!("script {}", script))?;
    }

    Ok(())
}

fn load_coverage(resolver: &PathResolver) -> Result<CoverageSpec> {
    let path = resolver.root().join("coverage.toml")?;
    let contents = resolver
        .read_to_string(&path)
        .context("read coverage.toml")?;
    let doc = contents
        .parse::<DocumentMut>()
        .context("parse coverage.toml")?;

    let mut coverage = HashMap::new();
    if let Some(table) = doc.get("coverage").and_then(|item| item.as_table()) {
        for (key, value) in table.iter() {
            let array = value
                .as_array()
                .ok_or_else(|| anyhow!("coverage.{key} must be an array"))?;
            let mut scripts = Vec::new();
            for item in array.iter() {
                let script = item
                    .as_str()
                    .ok_or_else(|| anyhow!("coverage.{key} entries must be strings"))?;
                scripts.push(script.to_string());
            }
            coverage.insert(key.to_string(), scripts);
        }
    }

    let extras = doc
        .get("extras")
        .and_then(|item| item.as_table())
        .and_then(|table| table.get("scripts"))
        .and_then(|item| item.as_array())
        .map(|array| {
            array
                .iter()
                .filter_map(|item| item.as_str().map(str::to_string))
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    Ok(CoverageSpec { coverage, extras })
}

fn collect_scripts(spec: &CoverageSpec) -> Result<Vec<String>> {
    let mut scripts: BTreeSet<String> = BTreeSet::new();
    for paths in spec.coverage.values() {
        for path in paths {
            scripts.insert(path.to_string());
        }
    }
    for extra in &spec.extras {
        scripts.insert(extra.to_string());
    }
    if scripts.is_empty() {
        return Err(anyhow!("coverage.toml did not list any scripts"));
    }
    Ok(scripts.into_iter().collect())
}

fn load_scripts(
    resolver: &PathResolver,
    scripts: &[String],
) -> Result<HashMap<String, Vec<Step>>> {
    let mut out = HashMap::new();
    let placeholders = command_placeholders();
    for script in scripts {
        let path = resolver
            .root()
            .join(script)
            .with_context(|| format!("resolve script {}", script))?;
        let template = resolver
            .read_to_string(&path)
            .with_context(|| format!("read script {}", script))?;
        let rendered = apply_placeholders(&template, &placeholders)
            .with_context(|| format!("render script {}", script))?;
        let steps = oxdock_parser::parse_script(&rendered)
            .with_context(|| format!("parse script {}", script))?;
        out.insert(script.to_string(), steps);
    }
    Ok(out)
}

fn apply_placeholders(template: &str, placeholders: &HashMap<&'static str, String>) -> Result<String> {
    let mut rendered = template.to_string();
    for (key, value) in placeholders {
        rendered = rendered.replace(key, value);
    }
    for key in ["@RUN_CMD@", "@RUN_BG_CMD@", "@BG_WAIT_CMD@", "@CAPTURE_RUN_CMD@"] {
        if rendered.contains(key) {
            return Err(anyhow!("script contains unresolved placeholder {key}"));
        }
    }
    Ok(rendered)
}

fn command_placeholders() -> HashMap<&'static str, String> {
    #[allow(clippy::disallowed_macros)]
    let run_cmd = if cfg!(windows) {
        "echo %FOO%> run.txt".to_string()
    } else {
        "printf %s \"$FOO\" > run.txt".to_string()
    };

    // Background command should stay alive long enough for foreground steps to complete.
    #[allow(clippy::disallowed_macros)]
    let bg_cmd = if cfg!(windows) {
        "ping -n 3 127.0.0.1 > NUL & echo %FOO%> bg.txt".to_string()
    } else if is_isolated() {
        "sleep 1 && printf %s \"$FOO\" > bg.txt".to_string()
    } else {
        "sleep 0.2 && printf %s \"$FOO\" > bg.txt".to_string()
    };

    #[allow(clippy::disallowed_macros)]
    let bg_wait = if cfg!(windows) {
        "ping -n 2 127.0.0.1 > NUL".to_string()
    } else {
        "sleep 0.4".to_string()
    };

    #[allow(clippy::disallowed_macros)]
    let capture_run = if cfg!(windows) {
        "echo hello".to_string()
    } else {
        "printf %s \"hello\"".to_string()
    };

    let mut placeholders = HashMap::new();
    placeholders.insert("@RUN_CMD@", run_cmd);
    placeholders.insert("@RUN_BG_CMD@", bg_cmd);
    placeholders.insert("@BG_WAIT_CMD@", bg_wait);
    placeholders.insert("@CAPTURE_RUN_CMD@", capture_run);
    placeholders
}

fn assert_coverage(
    scripts: &HashMap<String, Vec<Step>>,
    spec: &CoverageSpec,
) -> Result<()> {
    let step_kinds = step_kind_variants().context("load StepKind variants")?;

    let mut missing = Vec::new();
    for kind in &step_kinds {
        if !spec.coverage.contains_key(kind) {
            missing.push(kind.clone());
        }
    }
    if !missing.is_empty() {
        missing.sort();
        return Err(anyhow!(
            "coverage.toml missing StepKind entries: {}",
            missing.join(", ")
        ));
    }

    for key in spec.coverage.keys() {
        if !step_kinds.contains(key) {
            return Err(anyhow!("coverage.toml has unknown StepKind {}", key));
        }
    }

    for (kind, script_list) in &spec.coverage {
        if script_list.is_empty() {
            return Err(anyhow!("coverage entry {} is empty", kind));
        }
        for script in script_list {
            let steps = scripts
                .get(script)
                .ok_or_else(|| anyhow!("coverage references unknown script {}", script))?;
            let script_kinds = step_kinds_in_steps(steps);
            if !script_kinds.contains(kind) {
                return Err(anyhow!(
                    "coverage {} expects {}, but script does not include it",
                    script,
                    kind
                ));
            }
        }
    }

    Ok(())
}

fn step_kind_variants() -> Result<HashSet<String>> {
    let workspace_root = discover_workspace_root().context("locate workspace root")?;
    let resolver = PathResolver::new(workspace_root.as_path(), workspace_root.as_path())?;
    let ast_path = workspace_root.join("crates/internal/oxdock-parser/src/ast.rs")?;
    let ast_source = resolver.read_to_string(&ast_path).context("read ast.rs")?;
    let expected = extract_step_kind_variants(&ast_source);
    if expected.is_empty() {
        return Err(anyhow!("failed to extract StepKind variants from ast.rs"));
    }
    Ok(expected.into_iter().collect())
}

fn step_kinds_in_steps(steps: &[Step]) -> HashSet<String> {
    steps
        .iter()
        .map(|step| step_kind_name(&step.kind).to_string())
        .collect()
}

fn run_case(
    case: &ScriptCase,
    steps: &[Step],
    script_path: &GuardedPath,
) -> Result<()> {
    let snapshot_temp = GuardedPath::tempdir().context("create snapshot tempdir")?;
    let snapshot = guard_root(&snapshot_temp);
    let local_temp = GuardedPath::tempdir().context("create local tempdir")?;
    let local = guard_root(&local_temp);

    (case.setup)(&snapshot, &local).context("setup case")?;

    let build_context = match case.build_context {
        BuildContext::Local => local.clone(),
        BuildContext::LocalSubdir(rel) => local
            .join(rel)
            .with_context(|| format!("resolve build context {}", rel))?,
        BuildContext::Snapshot => snapshot.clone(),
    };

    let result = run_steps_with_context(&snapshot, &build_context, steps);
    if let Some(expected) = case.expect_error {
        let err = result.unwrap_err();
        if !err.to_string().contains(expected) {
            return Err(anyhow!(
                "expected error containing {expected}, got: {err}"
            ));
        }
    } else {
        result.with_context(|| format!("run {}", script_path.display()))?;
    }

    (case.verify)(&snapshot, &local).context("verify case")?;
    Ok(())
}

fn guard_root(temp: &GuardedTempDir) -> GuardedPath {
    temp.as_guarded_path().clone()
}

fn read_trimmed(path: &GuardedPath) -> Result<String> {
    let resolver = PathResolver::new(path.root(), path.root())?;
    Ok(resolver.read_to_string(path)?.trim().to_string())
}

fn write_text(path: &GuardedPath, contents: &str) -> Result<()> {
    let resolver = PathResolver::new(path.root(), path.root())?;
    resolver.write_file(path, contents.as_bytes())?;
    Ok(())
}

fn create_dirs(path: &GuardedPath) -> Result<()> {
    let resolver = PathResolver::new(path.root(), path.root())?;
    resolver.create_dir_all(path)?;
    Ok(())
}

fn exists(root: &GuardedPath, rel: &str) -> Result<bool> {
    Ok(root.join(rel)?.exists())
}

fn git_cmd(repo: &GuardedPath) -> CommandBuilder {
    let mut cmd = CommandBuilder::new("git");
    cmd.arg("-C").arg(repo.as_path());
    cmd
}

fn setup_copy_inputs(_snapshot: &GuardedPath, local: &GuardedPath) -> Result<()> {
    write_text(&local.join("source.txt")?, "from build")?;
    Ok(())
}

fn setup_symlink_inputs(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    let target_dir = snapshot.join("target_dir")?;
    create_dirs(&target_dir)?;
    write_text(&target_dir.join("inner.txt")?, "symlink target")?;
    Ok(())
}

fn setup_copy_git(_snapshot: &GuardedPath, local: &GuardedPath) -> Result<()> {
    let repo = local.join("repo")?;
    create_dirs(&repo)?;
    write_text(&repo.join("hello.txt")?, "git hello")?;

    git_cmd(&repo)
        .arg("init")
        .arg("-q")
        .status()
        .context("git init failed")?;
    git_cmd(&repo)
        .arg("add")
        .arg(".")
        .status()
        .context("git add failed")?;
    ensure_git_identity(&repo).context("ensure git identity")?;
    git_cmd(&repo)
        .arg("commit")
        .arg("-m")
        .arg("initial")
        .status()
        .context("git commit failed")?;

    Ok(())
}

fn verify_workdir(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    assert_eq!(
        read_trimmed(&snapshot.join("workspace/note.txt")?)?,
        "hi"
    );
    Ok(())
}

fn verify_workspace(snapshot: &GuardedPath, local: &GuardedPath) -> Result<()> {
    assert_eq!(read_trimmed(&snapshot.join("snap.txt")?)?, "snap");
    assert_eq!(read_trimmed(&snapshot.join("snap2.txt")?)?, "snap2");
    assert_eq!(read_trimmed(&local.join("local.txt")?)?, "local");
    Ok(())
}

fn verify_env(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    assert_eq!(read_trimmed(&snapshot.join("env.txt")?)?, "value=bar");
    Ok(())
}

fn verify_run(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    assert_eq!(read_trimmed(&snapshot.join("run.txt")?)?, "bar");
    Ok(())
}

fn verify_run_bg(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    assert_eq!(read_trimmed(&snapshot.join("bg.txt")?)?, "bar");
    Ok(())
}

fn verify_echo(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    assert_eq!(read_trimmed(&snapshot.join("echo.txt")?)?, "hello");
    Ok(())
}

fn verify_copy(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    assert_eq!(read_trimmed(&snapshot.join("copied.txt")?)?, "from build");
    Ok(())
}

fn verify_symlink(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    let linked = snapshot.join("linked/inner.txt")?;
    assert_eq!(read_trimmed(&linked)?, "symlink target");
    Ok(())
}

fn verify_mkdir(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    if !snapshot.join("a/b/c")?.exists() {
        return Err(anyhow!("missing directory a/b/c"));
    }
    Ok(())
}

fn verify_ls(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    let items = snapshot.join("items")?;
    let ls_content = read_trimmed(&snapshot.join("ls.txt")?)?;
    let mut lines: Vec<_> = ls_content.lines().map(str::to_string).collect();
    if lines.is_empty() {
        return Err(anyhow!("LS output missing header"));
    }
    let expected_header = format!(
        "{}:",
        PathResolver::new(snapshot.root(), snapshot.root())?
            .canonicalize(&items)?
            .display()
    );
    assert_eq!(lines.remove(0), expected_header);
    for entry in ["a.txt", "b.txt"] {
        if !lines.contains(&entry.to_string()) {
            return Err(anyhow!("LS output missing {}", entry));
        }
    }
    Ok(())
}

fn verify_cwd(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    let expected = PathResolver::new(snapshot.root(), snapshot.root())?
        .canonicalize(&snapshot.join("a/b")?)?
        .display()
        .to_string();
    assert_eq!(read_trimmed(&snapshot.join("a/b/pwd.txt")?)?, expected);
    Ok(())
}

fn verify_cat(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    assert_eq!(read_trimmed(&snapshot.join("out.txt")?)?, "hello");
    Ok(())
}

fn verify_write(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    assert_eq!(read_trimmed(&snapshot.join("note.txt")?)?, "hello");
    Ok(())
}

fn verify_capture_to_file(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    assert_eq!(read_trimmed(&snapshot.join("out.txt")?)?, "hello");
    Ok(())
}

fn verify_copy_git(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    assert_eq!(
        read_trimmed(&snapshot.join("out_hello.txt")?)?,
        "git hello"
    );
    Ok(())
}

fn verify_hash_sha256(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    let hash = read_trimmed(&snapshot.join("hash.txt")?)?;
    let mut hasher = Sha256::new();
    hasher.update(b"hello");
    let expected_hash = format!("{:x}", hasher.finalize());
    assert_eq!(hash, expected_hash);
    Ok(())
}

fn verify_guards(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    if !exists(snapshot, "guarded.txt")? {
        return Err(anyhow!("missing guarded.txt"));
    }
    if !exists(snapshot, "guarded_not.txt")? {
        return Err(anyhow!("missing guarded_not.txt"));
    }
    if !exists(snapshot, "guarded_or.txt")? {
        return Err(anyhow!("missing guarded_or.txt"));
    }
    if exists(snapshot, "guarded_skip.txt")? {
        return Err(anyhow!("guarded_skip.txt should not exist"));
    }

    #[allow(clippy::disallowed_macros)]
    if cfg!(windows) {
        if !exists(snapshot, "guarded_windows.txt")? {
            return Err(anyhow!("missing guarded_windows.txt"));
        }
        if exists(snapshot, "guarded_unix.txt")? {
            return Err(anyhow!("guarded_unix.txt should not exist"));
        }
    } else {
        if !exists(snapshot, "guarded_unix.txt")? {
            return Err(anyhow!("missing guarded_unix.txt"));
        }
        if exists(snapshot, "guarded_windows.txt")? {
            return Err(anyhow!("guarded_windows.txt should not exist"));
        }
    }

    Ok(())
}

fn verify_scopes(snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    assert_eq!(
        read_trimmed(&snapshot.join("scoped/inner.txt")?)?,
        "inner"
    );
    assert_eq!(
        read_trimmed(&snapshot.join("outer.txt")?)?,
        "outer"
    );
    assert_eq!(read_trimmed(&snapshot.join("env.txt")?)?, "scope=");
    Ok(())
}

fn script_cases() -> HashMap<&'static str, ScriptCase> {
    let mut cases = HashMap::new();
    cases.insert(
        "scripts/workdir.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_workdir,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/workspace.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_workspace,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/env.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_env,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/run.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_run,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/run_bg.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_run_bg,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/echo.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_echo,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/copy.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_copy_inputs,
            verify: verify_copy,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/symlink.oxdock",
        ScriptCase {
            build_context: BuildContext::Snapshot,
            setup: setup_symlink_inputs,
            verify: verify_symlink,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/mkdir.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_mkdir,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/ls.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_ls,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/cwd.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_cwd,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/cat.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_cat,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/write.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_write,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/capture_to_file.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_capture_to_file,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/copy_git.oxdock",
        ScriptCase {
            build_context: BuildContext::LocalSubdir("repo"),
            setup: setup_copy_git,
            verify: verify_copy_git,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/hash_sha256.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_hash_sha256,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/exit.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: setup_noop,
            expect_error: Some("EXIT requested with code 5"),
        },
    );
    cases.insert(
        "scripts/guards.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_guards,
            expect_error: None,
        },
    );
    cases.insert(
        "scripts/scopes.oxdock",
        ScriptCase {
            build_context: BuildContext::Local,
            setup: setup_noop,
            verify: verify_scopes,
            expect_error: None,
        },
    );
    cases
}

fn setup_noop(_snapshot: &GuardedPath, _local: &GuardedPath) -> Result<()> {
    Ok(())
}

fn extract_step_kind_variants(source: &str) -> Vec<String> {
    let mut variants = Vec::new();
    let mut in_enum = false;

    for line in source.lines() {
        let trimmed = line.trim_start();
        if !in_enum {
            if trimmed.starts_with("pub enum StepKind") {
                in_enum = true;
            }
            continue;
        }

        if trimmed == "}" {
            break;
        }
        if trimmed.is_empty() || trimmed.starts_with('#') || trimmed.starts_with('/') {
            continue;
        }

        let name: String = trimmed
            .chars()
            .take_while(|ch| ch.is_ascii_alphanumeric() || *ch == '_')
            .collect();
        if !name.is_empty() && name.chars().next().map(|ch| ch.is_ascii_uppercase()) == Some(true)
        {
            variants.push(name);
        }
    }

    variants
}

fn step_kind_name(kind: &StepKind) -> &'static str {
    match kind {
        StepKind::Workdir(_) => "Workdir",
        StepKind::Workspace(_) => "Workspace",
        StepKind::Env { .. } => "Env",
        StepKind::Run(_) => "Run",
        StepKind::Echo(_) => "Echo",
        StepKind::RunBg(_) => "RunBg",
        StepKind::Copy { .. } => "Copy",
        StepKind::Symlink { .. } => "Symlink",
        StepKind::Mkdir(_) => "Mkdir",
        StepKind::Ls(_) => "Ls",
        StepKind::Cwd => "Cwd",
        StepKind::Cat(_) => "Cat",
        StepKind::Write { .. } => "Write",
        StepKind::CaptureToFile { .. } => "CaptureToFile",
        StepKind::CopyGit { .. } => "CopyGit",
        StepKind::HashSha256 { .. } => "HashSha256",
        StepKind::Exit(_) => "Exit",
    }
}
