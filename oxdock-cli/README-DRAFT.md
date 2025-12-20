# OxDock (CLI)

`oxdock-cli` is a small CLI / library that runs OxDock DSL scripts against an isolated temporary workspace and can also be used to run against the main repository tree.

Its primary use case is to provide the ability to script together an environment with the same syntax that is used in the [build-time macros](../oxdock-buildtime-macros/).

## Quick features

- Create an isolated, temporary workspace and run a script inside it.
- Drop into an interactive shell inside the temporary workspace with `oxdock --shell` (requires a TTY).
- Run a DSL script via `--script <path>` or by piping a script into stdin.
- Expose the real workspace to scripts via `WORKSPACE LOCAL` / `WORKSPACE SNAPSHOT` so steps can target either the temporary workspace or the live repo.

## What it is not

OxDock does not enforce pinned dependency resolution on its own.  It strives to be a very simple, zero-cost, bring-your-own-config, orchestration layer instead.

## Common usage

Run a script file:
```sh
oxdock --script ./build.oxfile
```

Pipe a script into the CLI:
```sh
cat my-script.oxfile | oxdock
```

Drop into a shell inside the temporary workspace (interactive):
```sh
oxdock --shell
```

## Notes

- When invoked from Cargo builds the CLI will normally detect the manifest dir; if you run the installed binary from other locations, you can influence where the workspace root is discovered with the `OXDOCK_WORKSPACE_ROOT` environment variable.
- The CLI uses the same DSL and runtime as the `oxdock-core` crate; it sets a separate `CARGO_TARGET_DIR` for nested cargo invocations to avoid collisions with the outer build.

Integration with compile-time macros
- For compile-time embedding, see `oxdock-buildtime-macros::embed!` which runs the same DSL during `build.rs`/proc-macro time and produces an `out_dir` whose files are embedded directly into a generated struct (no additional runtime dependencies).

Examples
See the repository `examples/` folder for sample scripts and an `embed_demo.rs` that demonstrates how the compile-time macro and CLI work together.
