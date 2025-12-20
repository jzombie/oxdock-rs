# OxDock Workspace Guardrails

## Linting

Do not override linter rules in any crate except `oxdock-fs` or `oxdock-process`, and never add global/blanket overrides.

- **Naming**: Do not prefix parameter names with `_` to silence unused warnings. Prefer using the value (e.g., `let _ = param;`) or a localized `#[allow(unused_variables)]` if absolutely necessary.

- **Strategy**: When addressing clippy warnings, prefer localized `#[allow(...)]` on specific items (functions/impls/structs) instead of crate-level `#![allow(...)]`.
    - Avoid blanket overrides on traits or impl blocks if the lint only applies to a subset of methods; apply the suppression to the individual methods instead.
    - For imports, split the `use` statement so that `#[allow(...)]` only applies to the specific disallowed item.
    - For enums/structs, apply `#[allow(...)]` to the specific variant or field that uses the disallowed type, not the whole type definition.
- **Scope**: Do not perform repository-wide or file-wide blanket changes to satisfy lints; limit edits to the minimal, justified scope.
- **Enforcement**:
    - `clippy.toml` is used to `deny` `disallowed_macros` (like `std::cfg`), `disallowed_methods`, and `disallowed_types`.
    - `[workspace.lints.clippy]` in `Cargo.toml` enforces these rules workspace-wide.
    - Crates should not override these defaults unless absolutely necessary (rare).

## Miri & Isolation

Prefer encapsulation over scattered platform/test-runner checks.

- **Principle**: The `oxdock-fs` and `oxdock-process` crates should contain any logic that needs to behave differently when running under Miri or other isolated test runners. Other crates should not have to conditionally change behavior for Miri (tests *can* have Miri skips, but it is very discouraged, and they *MUST* include the reason why they are skipped).
-- **Implementation**:
    - Use `oxdock_fs::is_isolated()` instead of `cfg!(miri)` in implementation code. This funnels test-runner knowledge into a single, reviewable place.
    - Do not use `#[cfg(miri)]` attributes outside of `crates/internal/oxdock-fs` and `crates/internal/oxdock-process`, except for test-only `#[cfg_attr(miri, ignore = "...")]` skips that include a reason.
- **Exceptions**:
    - Implementation crates (`oxdock-fs`, `oxdock-process`) may legitimately use `cfg!(...)` or `#[cfg(...)]` internally.
    - Use narrow, localized `#[allow(clippy::disallowed_macros)]` or `#[allow(clippy::disallowed_methods, clippy::disallowed_types)]` on specific functions/items to keep the rest of the crate provably conformant.

## Filesystem & Process

- **Filesystem**: Use the `oxdock-fs` abstractions (`GuardedPath`/`UnguardedPath`, `PathResolver`, `GuardedPath::tempdir`) instead of raw `std::fs` for guarded paths; keep paths under their guards.
    - Do not use `tempfile` directly in any crate other than `oxdock-fs`.
- **Process Execution**: Use `oxdock-process` abstractions instead of raw `std::process::Command`.
    - Logic handling process execution differences (e.g. Miri vs Native) belongs in `oxdock-process`.

## Testing & Layout

- **Testing**: Prefer `cargo test --workspace --tests` to cover all crates; fixtures for the macros live under `crates/internal/oxdock-workspace-tests/fixtures/integration/buildtime_macros`.
- **Workspace layout**: Internal crates live under `crates/internal`; the CLI & build-time macros sit at the workspace root.
