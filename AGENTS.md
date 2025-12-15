AGENTS – workspace guardrails

- Lints: Do not override linter rules in any crate except `oxdock-fs`, and never add global/blanket overrides.
- Filesystem: Use the `oxdock-fs` abstractions (`GuardedPath`/`UnguardedPath`, `PathResolver`, `GuardedPath::tempdir`) instead of raw `std::fs` for guarded paths; keep paths under their guards.
- Testing: Prefer `cargo test --workspace --tests` to cover all crates; fixtures for the macros live under `oxdock-buildtime-macros/tests/fixtures`.
- Workspace layout: Internal crates live under `crates/internal`; the CLI, build-time macros, and fixtures sit at the workspace root.
