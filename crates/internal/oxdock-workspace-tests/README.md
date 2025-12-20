# oxdock-workspace-tests

Workspace-level fixtures and a libtest-mimic harness.

- Fixtures live in `fixtures/<name>/` as standalone Cargo projects.
- The harness auto-discovers directories with `Cargo.toml` and runs `cargo run --quiet`.
- Workspace dependencies are patched to local paths at runtime.

To add a fixture, create a new `fixtures/<name>/` folder with a `Cargo.toml` and source files.
