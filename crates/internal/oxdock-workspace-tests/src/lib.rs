//! Workspace-level test harness crate (not published).

pub mod harness {
    use anyhow::{Context, Result, anyhow};
    use libtest_mimic::Trial;
    use oxdock_fixture::FixtureBuilder;
    use oxdock_fs::{GuardedPath, PathResolver, command_path, discover_workspace_root};

    #[derive(Clone)]
    pub struct FixtureSpec {
        pub name: String,
        pub template: String,
    }

    #[derive(Clone)]
    pub struct FixtureCase {
        pub name: String,
        pub args: Vec<String>,
        pub env: Vec<(String, String)>,
        pub env_remove: Vec<String>,
        pub expect_success: bool,
        pub stdout_contains: Vec<String>,
        pub stdout_not_contains: Vec<String>,
        pub stderr_contains: Vec<String>,
        pub stderr_not_contains: Vec<String>,
    }

    #[derive(Clone)]
    pub struct HarnessConfig {
        pub fixtures_root: GuardedPath,
        pub exclude_root_dirs: Vec<String>,
        pub set_workspace_root_env: bool,
        pub set_temp_target_dir: bool,
        pub name: &'static str,
    }

    impl HarnessConfig {
        pub fn new(name: &'static str, fixtures_root: GuardedPath) -> Self {
            Self {
                fixtures_root,
                exclude_root_dirs: Vec::new(),
                set_workspace_root_env: false,
                set_temp_target_dir: false,
                name,
            }
        }
    }

    pub fn build_trials(
        resolver: &PathResolver,
        config: &HarnessConfig,
    ) -> Result<Vec<Trial>> {
        let fixtures = discover_fixtures(resolver, config)?;
        let tests: Vec<Trial> = fixtures
            .into_iter()
            .flat_map(|fixture| {
                let cases = load_fixture_cases(resolver, config, &fixture).unwrap_or_else(|err| {
                    eprintln!(
                        "{} harness failed to load expectations for {}: {err:#}",
                        config.name, fixture.name
                    );
                    std::process::exit(1);
                });
                let total_cases = cases.len();
                cases.into_iter().map(move |case| {
                    let config = config.clone();
                    let fixture = fixture.clone();
                    let name = case_display_name(&fixture.name, &case, total_cases);
                    Trial::test(name, move || run_fixture(&config, &fixture, &case))
                })
            })
            .collect();

        Ok(tests)
    }

    fn discover_fixtures(
        resolver: &PathResolver,
        config: &HarnessConfig,
    ) -> Result<Vec<FixtureSpec>> {
        let mut fixtures = Vec::new();
        discover_fixtures_recursive(
            resolver,
            &config.fixtures_root,
            "",
            &config.exclude_root_dirs,
            &mut fixtures,
        )?;
        fixtures.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(fixtures)
    }

    fn load_fixture_cases(
        resolver: &PathResolver,
        config: &HarnessConfig,
        spec: &FixtureSpec,
    ) -> Result<Vec<FixtureCase>> {
        let expectations_path = config
            .fixtures_root
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

    fn run_fixture(
        config: &HarnessConfig,
        spec: &FixtureSpec,
        case: &FixtureCase,
    ) -> std::result::Result<(), libtest_mimic::Failed> {
        run_fixture_inner(config, spec, case)
            .map_err(|err| libtest_mimic::Failed::from(format!("{err:#}")))
    }

    fn run_fixture_inner(
        config: &HarnessConfig,
        spec: &FixtureSpec,
        case: &FixtureCase,
    ) -> Result<()> {
        let workspace_root = discover_workspace_root().context("failed to locate workspace root")?;

        let mut builder = FixtureBuilder::new(spec.template.as_str())
            .context("failed to load fixture template")?
            .with_workspace_manifest_root(workspace_root.as_path());
        if config.set_workspace_root_env {
            builder = builder.with_workspace_root(workspace_root.as_path());
        }

        let fixture = builder
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

        let temp_target = if config.set_temp_target_dir {
            Some(GuardedPath::tempdir().context("create temp target dir")?)
        } else {
            None
        };

        let mut cmd = fixture.cargo();
        cmd.args(&case.args);
        if let Some(target) = &temp_target {
            cmd.env(
                "CARGO_TARGET_DIR",
                command_path(target.as_guarded_path()).into_owned(),
            );
        }
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
            has_content = true;
            if let Some(rest) = line.strip_prefix("case:") {
                current.name = rest.trim().to_string();
            } else if let Some(rest) = line.strip_prefix("args:") {
                current.args = rest.split_whitespace().map(|s| s.to_string()).collect();
            } else if let Some(rest) = line.strip_prefix("env:") {
                if let Some((k, v)) = rest.trim().split_once('=') {
                    current.env.push((k.to_string(), v.to_string()));
                } else {
                    return Err(anyhow!("line {}: env requires KEY=VALUE", idx + 1));
                }
            } else if let Some(rest) = line.strip_prefix("env_remove:") {
                current.env_remove.push(rest.trim().to_string());
            } else if let Some(rest) = line.strip_prefix("expect:") {
                match rest.trim() {
                    "success" => current.expect_success = true,
                    "failure" => current.expect_success = false,
                    other => return Err(anyhow!("line {}: invalid expect {}", idx + 1, other)),
                }
            } else if let Some(rest) = line.strip_prefix("stdout_contains:") {
                current.stdout_contains.push(rest.trim().to_string());
            } else if let Some(rest) = line.strip_prefix("stdout_not_contains:") {
                current.stdout_not_contains.push(rest.trim().to_string());
            } else if let Some(rest) = line.strip_prefix("stderr_contains:") {
                current.stderr_contains.push(rest.trim().to_string());
            } else if let Some(rest) = line.strip_prefix("stderr_not_contains:") {
                current.stderr_not_contains.push(rest.trim().to_string());
            } else {
                return Err(anyhow!("line {}: unrecognized directive", idx + 1));
            }
        }

        if has_content {
            cases.push(current);
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
        root: &GuardedPath,
        rel: &str,
        exclude_root_dirs: &[String],
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
            if name.starts_with('.') || name == "target" {
                continue;
            }
            if rel.is_empty() && exclude_root_dirs.contains(&name) {
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
                discover_fixtures_recursive(
                    resolver,
                    &candidate,
                    &rel_name,
                    exclude_root_dirs,
                    fixtures,
                )?;
            }
        }

        Ok(())
    }
}
