use anyhow::{Context, Result};
use libtest_mimic::{Arguments, Failed, Trial};
use oxdock_fs::{EntryKind, PathResolver, is_isolated};
use oxdock_parser::{Step, parse_braced_tokens, parse_script};
use proc_macro2::TokenStream;
use std::str::FromStr;

struct ParityCase {
    name: String,
    dsl: String,
    tokens: String,
}

fn main() {
    let args = Arguments::from_args();

    if is_isolated() {
        eprintln!(
            "Skipping DSL parity harness under isolated runner: requires filesystem access."
        );
        libtest_mimic::run(&args, Vec::new()).exit();
    }

    let resolver = PathResolver::from_manifest_env().unwrap_or_else(|err| {
        eprintln!("parity harness failed to resolve manifest dir: {err:#}");
        std::process::exit(1);
    });

    let cases = discover_cases(&resolver).unwrap_or_else(|err| {
        eprintln!("parity harness failed to discover cases: {err:#}");
        std::process::exit(1);
    });

    let tests: Vec<Trial> = cases
        .into_iter()
        .map(|case| {
            let name = case.name.clone();
            Trial::test(name, move || run_case(&case))
        })
        .collect();

    libtest_mimic::run(&args, tests).exit();
}

fn discover_cases(resolver: &PathResolver) -> Result<Vec<ParityCase>> {
    let fixtures_root = resolver.root().join("fixtures")?.join("parity")?;
    let entries = resolver
        .read_dir_entries(&fixtures_root)
        .context("failed to read parity fixtures directory")?;

    let mut cases = Vec::new();
    for entry in entries {
        let file_type = entry
            .file_type()
            .context("failed to read parity entry type")?;
        if !file_type.is_dir() {
            continue;
        }

        let name = entry.file_name().to_string_lossy().to_string();
        if name.starts_with('.') {
            continue;
        }

        let case_root = fixtures_root.join(&name)?;
        let dsl = case_root.join("dsl.txt")?;
        let tokens = case_root.join("tokens.rs")?;

        if matches!(resolver.entry_kind(&dsl), Ok(EntryKind::File))
            && matches!(resolver.entry_kind(&tokens), Ok(EntryKind::File))
        {
            let dsl_contents = resolver
                .read_to_string(&dsl)
                .with_context(|| format!("failed to read DSL fixture {name}"))?;
            let token_contents = resolver
                .read_to_string(&tokens)
                .with_context(|| format!("failed to read tokens fixture {name}"))?;
            cases.push(ParityCase {
                name,
                dsl: dsl_contents,
                tokens: token_contents,
            });
        }
    }

    cases.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(cases)
}

fn run_case(case: &ParityCase) -> std::result::Result<(), Failed> {
    run_case_inner(case).map_err(|err| Failed::from(err.to_string()))
}

fn run_case_inner(case: &ParityCase) -> Result<()> {
    let dsl_steps = parse_script(case.dsl.trim()).context("failed to parse DSL fixture")?;
    let token_stream = TokenStream::from_str(case.tokens.as_str())
        .map_err(|err| anyhow::anyhow!("failed to parse tokens fixture: {err}"))?;
    let token_steps =
        parse_braced_tokens(&token_stream).context("failed to parse tokens fixture")?;

    if dsl_steps != token_steps {
        anyhow::bail!(
            "AST mismatch for case {}.\n\nDSL AST:\n{}\n\nToken AST:\n{}",
            case.name,
            render_steps(&dsl_steps),
            render_steps(&token_steps)
        );
    }

    Ok(())
}

fn render_steps(steps: &[Step]) -> String {
    steps
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join("\n")
}
