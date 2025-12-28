use anyhow::Result;

fn main() -> Result<()> {
    runbook_cli::cli::run_from_env()
}
