use anyhow::{Context, Result};
use libtest_mimic::{Arguments, Failed, Trial};
use oxdock_fixture::FixtureBuilder;
use oxdock_fs::{PathResolver, discover_workspace_root, is_isolated};

struct FixtureSpec {
    name: String,
    template: String,
}

fn main() {
    let args = Arguments::from_args();

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
        .map(|fixture| {
            let name = fixture.name.clone();
            Trial::test(name, move || run_fixture(&fixture))
        })
        .collect();

    libtest_mimic::run(&args, tests).exit();
}

fn discover_fixtures(resolver: &PathResolver) -> Result<Vec<FixtureSpec>> {
    let fixtures_root = resolver.root().join("fixtures")?;
    let entries = resolver
        .read_dir_entries(&fixtures_root)
        .context("failed to read fixtures directory")?;

    let mut fixtures = Vec::new();
    for entry in entries {
        let file_type = entry
            .file_type()
            .context("failed to read fixtures entry type")?;
        if !file_type.is_dir() {
            continue;
        }

        let name = entry.file_name().to_string_lossy().to_string();
        if name.starts_with('.') {
            continue;
        }

        let candidate = fixtures_root.join(&name)?;
        let manifest = candidate.join("Cargo.toml")?;
        if resolver.entry_kind(&manifest).is_ok() {
            fixtures.push(FixtureSpec {
                name,
                template: candidate.to_string(),
            });
        }
    }

    fixtures.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(fixtures)
}

fn run_fixture(spec: &FixtureSpec) -> std::result::Result<(), Failed> {
    run_fixture_inner(spec).map_err(|err| Failed::from(err.to_string()))
}

fn run_fixture_inner(spec: &FixtureSpec) -> Result<()> {
    let workspace_root = discover_workspace_root().context("failed to locate workspace root")?;

    let fixture = FixtureBuilder::new(spec.template.as_str())
        .context("failed to load fixture template")?
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
            workspace_root.join("crates/internal/oxdock-fs")?.to_string(),
        )
        .with_path_dependency(
            "oxdock-process",
            workspace_root
                .join("crates/internal/oxdock-process")?
                .to_string(),
        )
        .instantiate()
        .context("failed to instantiate fixture")?;

    let mut cmd = fixture.cargo();
    cmd.arg("run").arg("--quiet");
    let output = cmd.output().context("failed to run fixture")?;

    if !output.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        anyhow::bail!(
            "fixture {} failed. stdout:\n{}\nstderr:\n{}",
            spec.name,
            stdout,
            stderr
        );
    }

    Ok(())
}
