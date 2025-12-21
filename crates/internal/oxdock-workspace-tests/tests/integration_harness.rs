use libtest_mimic::Arguments;
use oxdock_fs::PathResolver;
use oxdock_workspace_tests::harness::{HarnessConfig, build_trials};

fn main() {
    let mut args = Arguments::from_args();
    args.test_threads = Some(1);

    let resolver = PathResolver::from_manifest_env().unwrap_or_else(|err| {
        eprintln!("fixture harness failed to resolve manifest dir: {err:#}");
        std::process::exit(1);
    });

    let fixtures_root = resolver.root().join("fixtures").unwrap_or_else(|err| {
        eprintln!("fixture harness failed to resolve fixtures root: {err:#}");
        std::process::exit(1);
    });

    let mut config = HarnessConfig::new("integration", fixtures_root);
    config.exclude_root_dirs.push("commands".to_string());

    let tests = build_trials(&resolver, &config).unwrap_or_else(|err| {
        eprintln!("fixture harness failed to discover fixtures: {err:#}");
        std::process::exit(1);
    });

    libtest_mimic::run(&args, tests).exit();
}
