//! OxDock is a Dockerfile inspired build DSL for Rust. Embed scripts at compile time with macros, or run the same scripts as standalone CLI pipelines. Native. No containers. No daemon. No VM. All commands run identically on every OS, except RUN.
//!
//! Supports platform gating, async tasks, and piped workflows for custom pipelines.
//!
//! Scripts start in an ephemeral snapshot workspace and can switch to the local directory with `WORKSPACE LOCAL`. `oxdock_embed!` ships artifacts inside the binary with no heap use. `oxdock_prepare!` runs the same scripts for build only work with no runtime module.
#![allow(rustdoc::invalid_codeblock_attributes)]
#![doc = include_str!("../docs/command_reference.md")]

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
