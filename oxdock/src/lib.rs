#![allow(rustdoc::invalid_codeblock_attributes)]
#![doc = include_str!("../docs/crate_docs.md")]

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
