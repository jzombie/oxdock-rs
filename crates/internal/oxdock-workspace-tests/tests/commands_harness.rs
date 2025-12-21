use libtest_mimic::Arguments;
use oxdock_fs::{PathResolver, is_isolated};
use oxdock_workspace_tests::harness::{HarnessConfig, build_trials};

fn main() {
    let mut args = Arguments::from_args();
    args.test_threads = Some(1);

    if is_isolated() {
        eprintln!(
            "Skipping commands fixture harness under isolated runner: requires spawning cargo and filesystem access."
        );
        libtest_mimic::run(&args, Vec::new()).exit();
    }

    let resolver = PathResolver::from_manifest_env().unwrap_or_else(|err| {
        eprintln!("commands harness failed to resolve manifest dir: {err:#}");
        std::process::exit(1);
    });

    let fixtures_root = resolver
        .root()
        .join("fixtures")
        .and_then(|root| root.join("commands"))
        .unwrap_or_else(|err| {
            eprintln!("commands harness failed to resolve fixtures root: {err:#}");
            std::process::exit(1);
        });

    let mut config = HarnessConfig::new("commands", fixtures_root);
    config.set_workspace_root_env = true;
    config.set_temp_target_dir = true;
    config.case_config = Some(oxdock_workspace_tests::harness::CaseConfig {
        fixture_name: "ast_commands".to_string(),
        cases_dir: "cases".to_string(),
        case_env: "OXDOCK_AST_CASE".to_string(),
        coverage_env: Some("OXDOCK_AST_ONLY_COVERAGE".to_string()),
        coverage_case_name: "coverage".to_string(),
    });

    let tests = build_trials(&resolver, &config).unwrap_or_else(|err| {
        eprintln!("commands harness failed to discover fixtures: {err:#}");
        std::process::exit(1);
    });

    libtest_mimic::run(&args, tests).exit();
}
