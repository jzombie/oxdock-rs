AGENTS – workspace guardrails

- Lints: Do not override linter rules in any crate except `oxdock-fs`, and never add global/blanket overrides.
	- When addressing clippy warnings prefer localized `#[allow(...)]` on specific items (functions/impls/structs) instead of crate-level `#![allow(...)]`.
	- Do not perform repository-wide or file-wide blanket changes to satisfy lints; limit edits to the minimal, justified scope.
- Miri: In crates outside `crates/internal`, do not bypass, skip, or change behavior behind `cfg(miri)` (Miri must execute the same logic as other builds).
- Filesystem: Use the `oxdock-fs` abstractions (`GuardedPath`/`UnguardedPath`, `PathResolver`, `GuardedPath::tempdir`) instead of raw `std::fs` for guarded paths; keep paths under their guards.
	- Do not use `tempfile` directly in any crate other than `oxdock-fs`.
- Testing: Prefer `cargo test --workspace --tests` to cover all crates; fixtures for the macros live under `oxdock-buildtime-macros/tests/fixtures`.
- Workspace layout: Internal crates live under `crates/internal`; the CLI, build-time macros, and fixtures sit at the workspace root.

## TODO

- Update instructions to include "process" model for the global/blanket overrides, etc.
