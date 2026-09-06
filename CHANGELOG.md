# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/) and this project adheres to
(or is loosely based on) Semantic Versioning.

## [Unreleased]

### Stream-Native Pipeline Architecture

The refactored architecture eliminates all unbounded whole-stream buffering ($O(N)$ heap accumulation) across the DSL data path.

Every data pipeline handler now operates on fixed $O(1)$ stack buffers or $O(N_{\text{needle}})$ ring buffers, guaranteeing continuous chunked processing without EOF blocking.

**Pipeline Component Stream Audit**

| Component / Handler | Heap Allocation Complexity | Buffer Boundary | Streaming Mechanism |
| --- | --- | --- | --- |
| **`StreamingExpand` Engine** | $O(1)$ | Stack (`CHUNK_SIZE` B) + Key Buffer ($\le 1024$ B) | State machine parses `{{ env:KEY }}` boundaries across chunk boundaries; flushes literals immediately. |
| **`WRITE` / `APPEND` (stdin)** | $O(1)$ | Stack (`CHUNK_SIZE` B) | Single `open_write` / `open_append` file handle; streams `stdin` in 8 KB chunks with per-iteration `.flush()`. |
| **`EXPAND`** | $O(1)$ | Stack (`CHUNK_SIZE` B) | Feeds input stream through `StreamingExpand::process_bytes` and writes directly to downstream `stdout` handle. |
| **`HASH_SHA256`** | $O(1)$ | Stack (`CHUNK_SIZE` B) | Feeds chunked stream into `Sha256::update()`. Stdin pipe mode tees to `cx.out` per chunk without holding state. |
| **`ASSERT_STDOUT`** | $O(N_{\text{needle}})$ | Ring Buffer (`needle.len().max(1024)`) | Replaces unbounded `stdout_log: Vec<u8>`. `TeeWriter` pushes stdout bytes into step-indexed `SlidingWindow` observers. |
| **Metadata Commands (`WORKDIR`, `ENV`, `RUN`)** | $O(N_{\text{AST\_arg}})$ | In-Memory `String` / `Path` | Evaluates short AST strings for OS kernel syscalls (`execve`, `chdir`) prior to step launch. Executed outside I/O pipes. |

**Certified System Guarantees**

**Zero EOF Bottlenecks in Pipes**
Passive observer handlers (`HASH_SHA256`, `ASSERT_STDOUT`) and active transformers (`EXPAND`, `WRITE`) process 8 KB buffers and flush downstream immediately. Held-open or infinite process streams propagate without pipeline stalls or deadlocks.

**Strict Bounds on Assertion State**
Removing `ExecState.stdout_log` prevents memory leak vectors during long-running background tasks. Assertion state is capped to the byte length of registered template needles.

**Kernel-Mandated OS Metadata Boundaries**
Control metadata (`WORKDIR`, `ENV`, `Command::new` arguments) must be contiguous in RAM before invoking kernel primitives like `execve` or `set_current_dir`. These short AST strings are isolated from active stream pipelines and do not impact data stream throughput or memory bounds.

### Added

- **`EXPAND`** command: streams file or stdin through `StreamingExpand` template engine to stdout; supports explicit `KEY=val` overrides alongside `{{ env:KEY }}` interpolation
- **`StreamingExpand`** state machine: configurable `TemplateDelimiters`, handles `}}` split across 8 KB chunk boundaries, `pending_close_brace` / `pending_brace` deferred state, empty-input guard, and `MAX_PLACEHOLDER_SCAN` (1024 B) key buffer cap
- **`SlidingWindow`** ring buffer for `ASSERT_STDOUT`: replaces unbounded `stdout_log: Vec<u8>` with $O(N_{\text{needle}})$ matching; pre-registered per step index; re-expanded on `ENV`/`INHERIT_ENV` mutations via `update_needle` (preserves ring history)
- **`WorkspaceFs::open_read`**, **`open_write`**, **`open_append`** trait methods for streaming file I/O across Host, Miri, and Mock backends
- **`CHUNK_SIZE`** constant (`8192`) standardizing buffer sizes across all I/O handlers

### Refactored

- **`StreamingExpand`** replaces `expand_with_lookup` with byte-level state machine; `TemplateDelimiters` struct eliminates hardcoded `b"{{"` / `b"}}"` constants; `pending_close_brace` flag handles split `}}` across chunk boundaries; `flush()` emits deferred bytes; `process_bytes` returns early on empty input to preserve pending state
- **`WRITE` / `APPEND`** stdin path: replaced `read_to_end` with 8 KB chunk loop through `open_write` / `open_append`; single file handle per operation
- **`HASH_SHA256`** file path: replaced `read_file` with `open_read` + chunk loop; stdin mode tees to `cx.out` per chunk
- **`ASSERT_STDOUT`** handler: two-mode architecture — piped stdin (active consumption + tee) and step-scope (pre-registered `SlidingWindow`); empty stdin falls through to step-scope mode
- **`TeeWriter`**: removed `stdout_log: Arc<Mutex<Vec<u8>>>`; now pushes to `Arc<Mutex<HashMap<usize, SlidingWindow>>>` (`assert_windows`)
- **`ExecState`**: replaced `stdout_log` with `assert_windows: Arc<Mutex<HashMap<usize, SlidingWindow>>>`; all assertion windows pre-registered before execution loop; re-expanded on `ENV`/`INHERIT_ENV` mutations via `reexpand_assert_windows`
- **`MiriBackend::open_append`**: cursor seek to `existing.len()` prevents overwrite corruption
- All `8192` literals replaced with `CHUNK_SIZE` constant across handlers and `fs_ops`

### Fixed

- `StreamingExpand`: split `}}` across chunk boundaries now correctly detected; `pending_close_brace` flag prevents byte erasure; `flush()` emits deferred bytes; empty input preserves pending state
- `MiriBackend::open_append`: cursor initialized at end of existing data instead of position 0
- `ASSERT_STDOUT`: falls through to step-scope mode only when stdin is empty (not on failed match); error messages include ring buffer content for debugging
- `SlidingWindow::push_chunk`: eviction limit scales with `needle.len().max(MIN_RING_CAPACITY)` preventing premature history truncation for long needles

### Added — DSL language

- **`ASYNC` / `AWAIT` / `CANCEL`**: background tasks via `ASYNC <command>` / `ASYNC { block }` / `LET $t = ASYNC { ... }`; `AWAIT $t` blocks with bounded poll loop; synchronous `CANCEL $t` transitions `Running`/`Awaiting` → `Cancelled` (double-cancel is a no-op success)
- **`TIMEOUT` / `SLEEP`**: deadline control via `TIMEOUT <duration> <command>` / `TIMEOUT <duration> { block }` (`500ms`, `10s`, `2m`, `1h`, bare number = seconds, must be positive); `SLEEP <duration>` for delays
- **`READ_LINE $var`**: line-oriented stdin/file read into a variable
- **`EXPAND [<path>] [KEY=val ...]`**: template expansion of file or stdin (see stream architecture above); fails with an error when template expansion fails
- **`FOR $v IN <expr> { block }` / `FOR $k, $v IN <expr> { block }`**, **`IF <expr> { } ELSE IF / ELSE`**, **`LET $v = <expr>` / `ASSIGN`**: expression language with 4-tier precedence (`atom > comparison > && > ||`), `==` / `!=`, `!` negation, and `GLOB(...)` support
- **Guard expressions**: `!` / `not(...)` negation, `any(...)` / `all(...)` combinators, `eq(env:KEY,val)` / `neq(env:KEY,val)` comparisons, `bool:<val>` guards; `[guard]` prefixes apply to `LET`/`ENV`/`WORKDIR`/`WORKSPACE` blocks as well as commands
- **Unified block scoping**: every braced block (`IF`/`ELSE`, `FOR`, `TIMEOUT`, `ASYNC`, `WITH_IO`) pushes/pops variable, env, workdir, and workspace state via `push_scope`/`pop_scope`; scopes unwind on nested `EXIT`; files and pipes intentionally leak across boundaries
- **Single-site command registry**: `declare_commands!` in `oxdock-parser/src/commands.rs` generates `StepKind`, `lower_command`, and `CommandMeta` (leaf + structural `WITH_IO`/`FOR`/`IF`/`LET`/`ASYNC`/`AWAIT`/`CANCEL`/`TIMEOUT`); unknown-command errors include structural and case hints

### Added — crates, macros, tooling

- **`oxdock` facade crate** (new canonical entry point): re-exports `oxdock_core`, `oxdock_parser`, `oxdock_build`, and `oxdock!` / `oxdock_embed!` / `oxdock_prepare!`; `cli` feature re-exports the `oxdock-cli` runner; workspace `default-members = ["oxdock"]` so bare `cargo run` launches the CLI; binary moved from `oxdock-cli/src/main.rs`
- **CLI**: `--help` / `-h` prints usage and exits 0 (no process exit inside library `Options::parse`); positional script paths and `-` / `--script -` (stdin, the default when no script is given)
- **`oxdock!` proc-macro** (`oxdock-macros`): inline DSL alongside Rust code with `#var` host interpolation (avoids collision with DSL `$var`); `#ident` pairs are sanitized to placeholders, reconstructed via `script_from_braced_tokens`, parsed with `parse_script`; interpolated strings emit as literals to prevent runtime double-evaluation; covers `FOR`/`LET` blocks and `GLOB(#var)`
- **Crate/macro renames**: `oxdock-buildtime-helpers` → `oxdock-build`, `oxdock-buildtime-macros` → `oxdock-macros`; `embed!` → `oxdock_embed!`, `prepare!` → `oxdock_prepare!` (all call sites, docs, and fixtures updated)
- **`docs-gen` crate**: self-generating docs — command reference body (`docs/sections/07-command-body.md`) and index table (`docs/sections/06-command-reference.md`) from the same metadata registry (drift-proof: removed commands such as `RUN_BG` disappear automatically); root `README.md` and per-crate docs assembled via glob + template manifests; `pulldown-cmark` replaces the custom markdown parser
- **`oxdock-process`**: `spawn_interactive_shell` moved here (`OXDOCK_BANNER` env + `$1` shell path, no interpolation into `sh -c`); shell construction covered by tests; `oxdock-cli` runner is a thin delegate

### Changed

- Background execution model: `RUN_BG` semantics superseded by `ASYNC` task handles with `AWAIT`/`CANCEL`; `RAW_WRITE` superseded by `{{ env:KEY }}` variable interpolation
- `TemplateString` replaced by `Arg::String(String, bool quoted)` / `Arg::Expr(Expr)`; per-arg `quoted` tracking (quoted `--flags` stay positional)
- `Guard::{Platform, EnvExists, EnvEquals}` `invert: bool` field replaced by composable `not()` / `!` and `eq` / `neq` / `bool` guards
- Test layout: per-crate test files collapsed into single integration binaries (`tests/integration.rs` per crate); `slow-integration` feature gates long-running suites; in-process fast path for logic-test harnesses; reduced disk thrashing in fixtures
- `oxdock-fs` path handling normalized via `normalized_path` / `to_forward_slashes` (Windows CI parity for packaging invariant tests)

### Removed

- `RUN_BG` command (use `ASYNC`); `RAW_WRITE` command (use interpolation)
- `oxdock-buildtime-helpers` / `oxdock-buildtime-macros` crate names and bare `embed!` / `prepare!` macro names (see renames above)
- `ExecState::stdout_log: Vec<u8>` (replaced by step-indexed `SlidingWindow` observers); `expand_with_lookup` (replaced by `StreamingExpand`); custom markdown parser (replaced by `pulldown-cmark`); `DocSpec` in `docs-gen` (callers build init steps via `oxdock!`); orphaned `oxdock-process` `test_utils` module

### Fixed — beyond streaming

- Unknown-command diagnostics suggest structural statements and correct casing instead of a bare error
- Nested `EXIT` unwinds unified scopes so `LET`/`ENV`/`WORKDIR`/`WORKSPACE` do not leak
- `CANCEL` on completed/awaiting tasks and double-`CANCEL` succeed without killing unrelated tasks; `TIMEOUT` kills only the timed-out task
- Empty template input preserves `StreamingExpand` pending state instead of flushing deferred bytes

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
