use anyhow::{Context, Result, anyhow};
use libtest_mimic::{Arguments, Failed, Trial};
use oxdock_fixture::FixtureBuilder;
use oxdock_fs::{PathResolver, discover_workspace_root, is_isolated};

#[derive(Clone)]
struct FixtureSpec {
    name: String,
    template: String,
}

#[derive(Clone)]
struct FixtureCase {
    name: String,
    args: Vec<String>,
    env: Vec<(String, String)>,
    env_remove: Vec<String>,
    expect_success: bool,
    stdout_contains: Vec<String>,
    stdout_not_contains: Vec<String>,
    stderr_contains: Vec<String>,
    stderr_not_contains: Vec<String>,
}

fn main() {
    let mut args = Arguments::from_args();
    args.test_threads = Some(1);

    if is_isolated() {
        eprintln!(
            "Skipping workspace fixture harness under isolated runner: requires spawning cargo and filesystem access."
        );
        libtest_mimic::run(&args, Vec::new()).exit();
    }

    let resolver = PathResolver::from_manifest_env().unwrap_or_else(|err| {
        eprintln!("fixture harness failed to resolve manifest dir: {err:#}");
        std::process::exit(1);
    });

    let fixtures = discover_fixtures(&resolver).unwrap_or_else(|err| {
        eprintln!("fixture harness failed to discover fixtures: {err:#}");
        std::process::exit(1);
    });

    let tests: Vec<Trial> = fixtures
        .into_iter()
        .flat_map(|fixture| {
            let cases = load_fixture_cases(&resolver, &fixture).unwrap_or_else(|err| {
                eprintln!(
                    "fixture harness failed to load expectations for {}: {err:#}",
                    fixture.name
                );
                std::process::exit(1);
            });
            let total_cases = cases.len();
            cases.into_iter().map(move |case| {
                let fixture = fixture.clone();
                let name = case_display_name(&fixture.name, &case, total_cases);
                Trial::test(name, move || run_fixture(&fixture, &case))
            })
        })
        .collect();

    libtest_mimic::run(&args, tests).exit();
}

fn discover_fixtures(resolver: &PathResolver) -> Result<Vec<FixtureSpec>> {
    let fixtures_root = resolver.root().join("fixtures")?;
    let mut fixtures = Vec::new();
    discover_fixtures_recursive(resolver, &fixtures_root, "", &mut fixtures)?;

    fixtures.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(fixtures)
}

fn load_fixture_cases(resolver: &PathResolver, spec: &FixtureSpec) -> Result<Vec<FixtureCase>> {
    let expectations_path = resolver
        .root()
        .join("fixtures")?
        .join(&spec.name)?
        .join("expectations.txt")?;

    if resolver.entry_kind(&expectations_path).is_err() {
        return Ok(vec![FixtureCase::default_case()]);
    }

    let contents = resolver
        .read_to_string(&expectations_path)
        .context("failed to read expectations.txt")?;
    parse_expectations(&contents)
}

fn case_display_name(fixture_name: &str, case: &FixtureCase, total: usize) -> String {
    if total == 1 && case.name == "default" {
        fixture_name.to_string()
    } else {
        format!("{fixture_name}::{case}", case = case.name)
    }
}

fn run_fixture(spec: &FixtureSpec, case: &FixtureCase) -> std::result::Result<(), Failed> {
    run_fixture_inner(spec, case).map_err(|err| Failed::from(format!("{err:#}")))
}

fn run_fixture_inner(spec: &FixtureSpec, case: &FixtureCase) -> Result<()> {
    let workspace_root = discover_workspace_root().context("failed to locate workspace root")?;

    let fixture = FixtureBuilder::new(spec.template.as_str())
        .context("failed to load fixture template")?
        .with_workspace_manifest_root(workspace_root.as_path())
        .with_path_dependency(
            "oxdock-buildtime-macros",
            workspace_root.join("oxdock-buildtime-macros")?.to_string(),
        )
        .with_path_dependency(
            "oxdock-buildtime-helpers",
            workspace_root.join("oxdock-buildtime-helpers")?.to_string(),
        )
        .with_path_dependency(
            "oxdock-embed",
            workspace_root
                .join("crates/internal/oxdock-embed")?
                .to_string(),
        )
        .with_path_dependency(
            "oxdock-fs",
            workspace_root
                .join("crates/internal/oxdock-fs")?
                .to_string(),
        )
        .with_path_dependency(
            "oxdock-core",
            workspace_root
                .join("crates/internal/oxdock-core")?
                .to_string(),
        )
        .with_path_dependency(
            "oxdock-parser",
            workspace_root
                .join("crates/internal/oxdock-parser")?
                .to_string(),
        )
        .with_path_dependency(
            "oxdock-process",
            workspace_root
                .join("crates/internal/oxdock-process")?
                .to_string(),
        )
        .with_path_dependency("oxdock-cli", workspace_root.join("oxdock-cli")?.to_string())
        .instantiate()
        .context("failed to instantiate fixture")?;

    let mut cmd = fixture.cargo();
    cmd.args(&case.args);
    for (key, value) in &case.env {
        cmd.env(key, value);
    }
    for key in &case.env_remove {
        cmd.env_remove(key);
    }
    let output = cmd.output().context("failed to run fixture")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    if case.expect_success && !output.success() {
        anyhow::bail!(
            "fixture {} failed. stdout:\n{}\nstderr:\n{}",
            spec.name,
            stdout,
            stderr
        );
    }
    if !case.expect_success && output.success() {
        anyhow::bail!(
            "fixture {} unexpectedly succeeded. stdout:\n{}\nstderr:\n{}",
            spec.name,
            stdout,
            stderr
        );
    }

    assert_contains(&stdout, &case.stdout_contains, "stdout", spec)?;
    assert_not_contains(&stdout, &case.stdout_not_contains, "stdout", spec)?;
    assert_contains(&stderr, &case.stderr_contains, "stderr", spec)?;
    assert_not_contains(&stderr, &case.stderr_not_contains, "stderr", spec)?;

    Ok(())
}

impl FixtureCase {
    fn default_case() -> Self {
        Self {
            name: "default".to_string(),
            args: vec!["run".to_string(), "--quiet".to_string()],
            env: Vec::new(),
            env_remove: Vec::new(),
            expect_success: true,
            stdout_contains: Vec::new(),
            stdout_not_contains: Vec::new(),
            stderr_contains: Vec::new(),
            stderr_not_contains: Vec::new(),
        }
    }
}

fn parse_expectations(contents: &str) -> Result<Vec<FixtureCase>> {
    let mut cases = Vec::new();
    let mut current = FixtureCase::default_case();
    let mut has_content = false;

    for (idx, raw_line) in contents.lines().enumerate() {
        let line = raw_line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        if line == "---" {
            if has_content {
                cases.push(current);
                current = FixtureCase::default_case();
                has_content = false;
            }
            continue;
        }

        let (key, value) = line
            .split_once(':')
            .ok_or_else(|| anyhow!("invalid expectations line {}: {}", idx + 1, raw_line))?;
        let key = key.trim();
        let value = value.trim();

        match key {
            "case" | "name" => {
                current.name = value.to_string();
            }
            "args" => {
                current.args = value
                    .split_whitespace()
                    .map(|arg| arg.to_string())
                    .collect();
            }
            "env" => {
                let (env_key, env_value) = value.split_once('=').ok_or_else(|| {
                    anyhow!(
                        "invalid env entry on line {}: {} (expected KEY=VALUE)",
                        idx + 1,
                        raw_line
                    )
                })?;
                current
                    .env
                    .push((env_key.trim().to_string(), env_value.trim().to_string()));
            }
            "env_remove" => {
                current.env_remove.push(value.to_string());
            }
            "expect" => match value {
                "success" => current.expect_success = true,
                "failure" => current.expect_success = false,
                _ => {
                    return Err(anyhow!(
                        "invalid expect value on line {}: {}",
                        idx + 1,
                        raw_line
                    ));
                }
            },
            "stdout_contains" => current.stdout_contains.push(value.to_string()),
            "stdout_not_contains" => current.stdout_not_contains.push(value.to_string()),
            "stderr_contains" => current.stderr_contains.push(value.to_string()),
            "stderr_not_contains" => current.stderr_not_contains.push(value.to_string()),
            _ => {
                return Err(anyhow!(
                    "invalid expectations key on line {}: {}",
                    idx + 1,
                    raw_line
                ));
            }
        }

        has_content = true;
    }

    if has_content {
        cases.push(current);
    }

    if cases.is_empty() {
        return Err(anyhow!("expectations.txt did not define any cases"));
    }

    Ok(cases)
}

fn assert_contains(
    haystack: &str,
    needles: &[String],
    stream: &str,
    spec: &FixtureSpec,
) -> Result<()> {
    for needle in needles {
        if !haystack.contains(needle) {
            return Err(anyhow!(
                "fixture {} {} missing expected text: {}",
                spec.name,
                stream,
                needle
            ));
        }
    }
    Ok(())
}

fn assert_not_contains(
    haystack: &str,
    needles: &[String],
    stream: &str,
    spec: &FixtureSpec,
) -> Result<()> {
    for needle in needles {
        if haystack.contains(needle) {
            return Err(anyhow!(
                "fixture {} {} contained unexpected text: {}",
                spec.name,
                stream,
                needle
            ));
        }
    }
    Ok(())
}

fn discover_fixtures_recursive(
    resolver: &PathResolver,
    root: &oxdock_fs::GuardedPath,
    rel: &str,
    fixtures: &mut Vec<FixtureSpec>,
) -> Result<()> {
    let entries = resolver
        .read_dir_entries(root)
        .context("failed to read fixtures directory")?;

    for entry in entries {
        let file_type = entry
            .file_type()
            .context("failed to read fixtures entry type")?;
        if !file_type.is_dir() {
            continue;
        }

        let name = entry.file_name().to_string_lossy().to_string();
        if name.starts_with('.') || name == "target" || (rel.is_empty() && name == "commands") {
            continue;
        }

        let candidate = root.join(&name)?;
        let manifest = candidate.join("Cargo.toml")?;
        let rel_name = if rel.is_empty() {
            name.clone()
        } else {
            format!("{rel}/{name}")
        };

        if resolver.entry_kind(&manifest).is_ok() {
            fixtures.push(FixtureSpec {
                name: rel_name,
                template: candidate.to_string(),
            });
        } else {
            discover_fixtures_recursive(resolver, &candidate, &rel_name, fixtures)?;
        }
    }

    Ok(())
}
