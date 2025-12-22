# oxdock-buildtime-macros

This crate provides a compile-time proc-macro `embed!` that runs a OxDock DSL script during your crate's build to produce embedded assets.

What this crate does:

- Executes a OxDock script at compile-time (when possible) and materializes the script output into an `out_dir` inside the crate.
- Generates a lightweight struct that exposes the produced files with a filesystem-like API (for example, `MyAssets::get("path")` returns embedded file bytes) without requiring `std` at runtime.
- Runs the script in an isolated snapshot of the repository (via `git archive`) so outputs are reproducible and your working tree is not modified during generation.
- Fails the compilation if any scripted step fails (or if the script contains an `EXIT` with a non-zero code), making build-time generation explicit and visible.
- When no `.git` checkout is present (for example, a crates.io tarball), the macro requires a prebuilt `out_dir` rather than attempting to run the script.

Basic usage

```rust
use oxdock_buildtime_macros::embed;

embed! {
	name: DemoAssets,
	script: r#"
		WORKSPACE LOCAL
		WORKDIR client
		RUN npm run build
		WORKSPACE SNAPSHOT
		COPY client/dist prebuilt/client/dist
"#,
	out_dir: "prebuilt",
}

// You can also embed scripts using a Rust-style DSL without raw strings.
embed! {
    name: InlineAssets,
    script: {
        [env:PROFILE==release] {
            WORKDIR /client
            RUN npm run build
        }
        WORKDIR /
        COPY client/dist prebuilt/dist
    },
    out_dir: "prebuilt",
}
// Blocks use scoped environments: WORKDIR/WORKSPACE/ENV changes inside the braces
// reset automatically after the block ends, so temporary setup stays contained.

// At runtime you can use the generated struct directly:
// if let Some(bytes) = DemoAssets::get("client/dist/index.html") { ... }
```

See the parent repository for the DSL reference and examples: the macro runs the same OxDock DSL used by the `oxdock` CLI.
