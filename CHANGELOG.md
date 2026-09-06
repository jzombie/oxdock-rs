# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/) and this project adheres to
(or is loosely based on) Semantic Versioning.

## [0.8.0-alpha] - 2026-09-05

### Added

- `EXPAND` command for template expansion of a file or stdin to stdout, with `KEY=val` overrides alongside `{{ env:KEY }}` interpolation
- `ASYNC` / `AWAIT` / `CANCEL` for background tasks, including block form and `LET $t = ASYNC { ... }` handles
- `TIMEOUT <duration> <command|block>` and `SLEEP <duration>` for deadline control and delays
- `READ_LINE $var` for line-oriented reads into a variable
- `FOR` (value and key-value forms), `IF` / `ELSE IF` / `ELSE`, and `LET` / `ASSIGN` with expression support (`==` / `!=`, `!` negation, `&&` / `||`, `GLOB(...)`)
- Guard expressions: `!` / `not(...)`, `any(...)` / `all(...)`, `eq(...)` / `neq(...)`, `bool:<val>`; `[guard]` prefixes on `LET` / `ENV` / `WORKDIR` / `WORKSPACE` blocks
- Unified block scoping for braced blocks (`IF`, `FOR`, `TIMEOUT`, `ASYNC`, `WITH_IO`) with scope unwind on nested `EXIT`
- `oxdock` facade crate as the canonical entry point; bare `cargo run` launches the CLI
- CLI `--help` / `-h` usage output, positional script paths, and `-` / `--script -` stdin handling
- `oxdock!` proc-macro for inline DSL with `#var` host interpolation, including `FOR` / `LET` blocks and `GLOB(#var)`
- `WorkspaceFs::open_read` / `open_write` / `open_append` streaming file I/O across Host, Miri, and Mock backends

### Changed

- Data pipeline handlers (`WRITE` / `APPEND`, `EXPAND`, `HASH_SHA256`, `ASSERT_STDOUT`) stream in fixed-size chunks instead of buffering whole inputs
- `ASSERT_STDOUT` uses bounded per-step matching instead of an unbounded stdout log; windows are re-expanded on `ENV` / `INHERIT_ENV` mutations
- `RUN_BG` semantics superseded by `ASYNC` task handles with `AWAIT` / `CANCEL`; `RAW_WRITE` superseded by `{{ env:KEY }}` interpolation
- Per-arg quoting tracked via `Arg::String` / `Arg::Expr` (quoted `--flags` stay positional)
- `Guard` inversion replaced by composable `not()` / `!` and `eq` / `neq` / `bool` guards
- Single-site command registry generating step kinds, lowering, and metadata; unknown-command errors include structural and casing hints
- Crate renames: `oxdock-buildtime-helpers` to `oxdock-build`, `oxdock-buildtime-macros` to `oxdock-macros`; `embed!` / `prepare!` to `oxdock_embed!` / `oxdock_prepare!`
- Docs generated from the command registry; `pulldown-cmark` replaces the custom markdown parser
- `spawn_interactive_shell` moved to `oxdock-process`; CLI runner is a thin delegate
- Test layout collapsed to single integration binaries per crate with a `slow-integration` feature gate
- `oxdock-fs` path handling normalized for Windows CI parity
- Bump `syn` 2.0.119 → 3.0.4

### Removed

- `RUN_BG` command (use `ASYNC`); `RAW_WRITE` command (use interpolation)
- Old `oxdock-buildtime-helpers` / `oxdock-buildtime-macros` crate names and bare `embed!` / `prepare!` macro names
- Unbounded `ExecState::stdout_log`; legacy `expand_with_lookup`; custom markdown parser; `DocSpec` in `docs-gen`; orphaned `oxdock-process` `test_utils` module

### Fixed

- Template `}}` split across chunk boundaries is detected; empty input preserves pending expansion state
- `open_append` on the Miri backend appends instead of overwriting from position 0
- `ASSERT_STDOUT` falls through to step-scope matching only on empty stdin; error output includes buffered content for debugging
- Long-needle assertions no longer truncate history prematurely
- Unknown-command diagnostics suggest structural statements and correct casing
- Nested `EXIT` unwinds `LET` / `ENV` / `WORKDIR` / `WORKSPACE` scopes without leaking
- `CANCEL` on completed tasks and double-`CANCEL` succeed without affecting unrelated tasks; `TIMEOUT` kills only the timed-out task

## [0.7.0-alpha] - 2026-08-27

### Refactoring

- **oxdock-core**: Split `exec.rs` into focused modules: `handlers`, `fs_ops`, `io`, `pipe`, `state`, `steps`, and `tests` for better maintainability
- **oxdock-process**: Decomposed `lib.rs` into `builder`, `shell`, `child`, `contract`, `expand`, `shell_manager`, `synthetic`, and `builtin_env` modules

### Added

- `APPEND` command for cross-platform append-only file writes (ideal for GitHub Actions `$GITHUB_OUTPUT`, `$GITHUB_ENV`, `$GITHUB_STEP_SUMMARY`)
- GitHub Actions Integration section in README documenting `ECHO`, `RUN`, and `APPEND` patterns for workflow commands
- Markdown DSL parsing support (`oxdock-parser/src/markdown.rs`)
- `OXDOCK_EMBED_FINGERPRINT_SALT` environment variable for cache busting
- `ASSERT_STDOUT` and `ASSERT_ABSENT` command prototypes
- Docs conformance tests and packaging invariant tests
- Expanded README with comprehensive documentation

### Fixed

- Fuzz parity test failure: filter out strings that fail `proc_macro2` lexing instead of panicking

### Dependencies

- Bump `anyhow` 1.0.100 → 1.0.104
- Bump `libc` 0.2.178 → 0.2.189
- Bump `libtest-mimic` 0.8.1 → 0.8.2
- Bump `line-ending` 1.5 → 1.5.1
- Bump `pest`/`pest_derive` 2.8.4 → 2.9.0
- Bump `proc-macro2` 1.0.103 → 1.0.107
- Bump `proptest` 1.9.0 → 1.11.0
- Bump `quote` 1.0.42 → 1.0.47
- Bump `sha2` 0.10.9 → 0.11.0 (with API migration)
- Bump `syn` 2.0 → 2.0.119
- Bump `tempfile` 3.24.0 → 3.27.0
- Bump `toml_edit` 0.24.0 → 0.25.13
- Update all transitive dependencies via `cargo update`
