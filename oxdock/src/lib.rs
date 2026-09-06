//! Facade crate for OxDock: the canonical user-facing entry point.
//!
//! - The CLI runner (`run`, `execute`, `Options`, ...) lives in `oxdock-cli`
//!   and is re-exported here behind the `cli` feature (enabled by default),
//!   so `oxdock::run()` and `oxdock_cli::run()` execute identically.
//! - The build macros (`oxdock!`, `oxdock_embed!`, `oxdock_prepare!`) are
//!   re-exported unconditionally, along with the support crates (`oxdock_core`,
//!   `oxdock_parser`, `oxdock_build`) that macro expansions resolve against.

// Re-exported unconditionally so `oxdock!` expansions (which emit absolute
// `oxdock_parser::...` paths) resolve for consumers that depend only on `oxdock`,
// including `--no-default-features` (macros-only) builds.
pub use oxdock_build;
pub use oxdock_core;
pub use oxdock_parser;

pub use oxdock_macros::{oxdock, oxdock_embed, oxdock_prepare};

// CLI runner logic stays in `oxdock-cli`; this re-export is the identical entry point.
#[cfg(feature = "cli")]
pub use oxdock_cli::{
    ExecutionResult, Guard, Options, ScriptSource, Step, StepKind, execute, execute_with_result,
    parse_script, run, run_script, run_steps, run_steps_with_context,
    run_steps_with_context_result, shell_program, usage,
};
