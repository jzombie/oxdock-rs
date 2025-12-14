# OxDock (CLI)

`oxdock-cli` is a small CLI / library that runs Doc‑Ox DSL scripts against a repository snapshot (separate directory tree) and can be used to run against the main repository tree.

Its primary use case is to provide the ability to script together an environment with the same syntax that is used in the [build-time macros](../oxdock-buildtime-macros/).

## Quick features

- Create a reproducible, isolated snapshot of your repository (using `git archive`) and run a script inside that snapshot. The snapshot does not contain .git VCS directly and can be helpful for publishing staged artifacts to Cargo without using the `--allow-dirty` flag.
- Drop into an interactive snapshot shell with `oxdock --shell` (requires a TTY).
- Run a DSL script via `--script <path>` or by piping a script into stdin.
- Expose the real workspace to scripts via `WORKSPACE LOCAL` / `WORKSPACE SNAPSHOT` so steps can target either the isolated snapshot or the live repo.

## Common usage

Run a script file:
```sh
oxdock --script ./build.docox
```

Pipe a script into the CLI:
```sh
cat my-script.docox | oxdock
```

Drop into a shell inside the snapshot (interactive):
```sh
oxdock --shell
```

## Notes

- When invoked from Cargo builds the CLI will normally detect the manifest dir; if you run the installed binary from other locations, you can influence where the workspace root is discovered with the `OXDOCK_WORKSPACE_ROOT` environment variable.
- The CLI uses the same DSL and runtime as the `oxdock-dsl` crate; it sets a separate `CARGO_TARGET_DIR` for nested cargo invocations to avoid collisions with the outer build.

Integration with compile-time macros
- For compile-time embedding, see `oxdock-buildtime-macros::embed!` which runs the same DSL during `build.rs`/proc-macro time and produces an `out_dir` consumed by `rust-embed`.

Examples
See the repository `examples/` folder for sample scripts and an `embed_demo.rs` that demonstrates how the compile-time macro and CLI work together.
