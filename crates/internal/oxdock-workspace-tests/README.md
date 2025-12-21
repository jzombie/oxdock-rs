# oxdock-workspace-tests

Workspace-level fixtures and a [libtest-mimic](https://crates.io/crates/libtest-mimic) harness.

- Fixtures live under `fixtures/` as standalone Cargo projects (including nested subdirectories).
- The harness auto-discovers directories with `Cargo.toml` and runs `cargo run --quiet` by default.
- Workspace dependencies are patched to local paths at runtime.

To add a fixture, create a new `fixtures/<name>/` (or nested) folder with a `Cargo.toml` and source files.

## Fixture expectations

Fixtures can define an `expectations.txt` file to customize commands, environment,
and expected output. When present, the harness runs one test per case in the file.
Without it, the harness defaults to `cargo run --quiet` and expects success.

`expectations.txt` format:

- `case: <name>` (optional, defaults to `default`)
- `args: <cargo args>` (optional, defaults to `run --quiet`)
- `env: KEY=VALUE` (optional, repeatable)
- `env_remove: KEY` (optional, repeatable)
- `expect: success|failure` (optional, defaults to `success`)
- `stdout_contains: <text>` (optional, repeatable)
- `stdout_not_contains: <text>` (optional, repeatable)
- `stderr_contains: <text>` (optional, repeatable)
- `stderr_not_contains: <text>` (optional, repeatable)
- `---` separates multiple cases

## Integration fixtures

Non-parity fixtures live under `fixtures/integration/`, for example:

- `fixtures/integration/build_from_manifest/`
- `fixtures/integration/buildtime_macros/<name>/`

## Build-time macro fixtures

Fixtures used by the build-time macro integration tests live under
`fixtures/integration/buildtime_macros/<name>/`. These are exercised by the same fixture
harness via `expectations.txt` cases.

## DSL parity cases

Parity fixtures live under `fixtures/parity/<case>/` and compare string DSL to token DSL.

- `dsl.txt` holds the string-based DSL.
- `tokens.rs` holds the braced-token version (the contents of a `script: { ... }` block).
- If `expect_error.txt` exists, it must contain the full parser error message verbatim.

The parity harness parses both and asserts their ASTs match.
