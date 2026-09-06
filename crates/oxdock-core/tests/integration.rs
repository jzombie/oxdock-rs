// Single integration test binary for oxdock-core: every former
// `tests/<name>.rs` target now lives here as a submodule so the crate
// links its test dependencies once instead of once per file. No test was
// added, removed, or altered in the move — each module is byte-identical
// to its original file.
#[path = "integration/commands.rs"]
mod commands;
#[path = "integration/guard_isolation.rs"]
mod guard_isolation;
#[path = "integration/incremental.rs"]
mod incremental;
#[path = "integration/key_path.rs"]
mod key_path;
#[path = "integration/phase_boundary.rs"]
mod phase_boundary;
#[path = "integration/unquoted_syntax.rs"]
mod unquoted_syntax;
