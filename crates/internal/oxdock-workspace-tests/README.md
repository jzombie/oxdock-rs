# oxdock-workspace-tests

Workspace-level fixtures and a libtest-mimic harness.

- Fixtures live in `fixtures/<name>/` as standalone Cargo projects.
- The harness auto-discovers directories with `Cargo.toml` and runs `cargo run --quiet`.
- Workspace dependencies are patched to local paths at runtime.

To add a fixture, create a new `fixtures/<name>/` folder with a `Cargo.toml` and source files.

## DSL parity cases

Parity fixtures live under `fixtures/parity/<case>/` and compare string DSL to token DSL.

- `dsl.txt` holds the string-based DSL.
- `tokens.rs` holds the braced-token version (the contents of a `script: { ... }` block).
- If `expect_error.txt` exists, its contents must appear in the parser error message.

The parity harness parses both and asserts their ASTs match.
