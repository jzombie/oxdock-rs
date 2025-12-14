# doc-ox-buildtime-macros

This crate provides a compile-time proc-macro `embed!` that runs a Doc‑Ox DSL script during your crate's build to produce embedded assets.

What this crate does:

- Executes a Doc‑Ox script at compile-time (when possible) and materializes the script output into an `out_dir` inside the crate.
- Uses `rust-embed` to generate a struct that exposes the produced files with a filesystem-like API (for example, `MyAssets::get("path")` returns embedded file bytes).
- Runs the script in an isolated snapshot of the repository (via `git archive`) so outputs are reproducible and your working tree is not modified during generation.
- Fails the compilation if any scripted step fails (or if the script contains an `EXIT` with a non-zero code), making build-time generation explicit and visible.
- When no `.git` checkout is present (for example, a crates.io tarball), the macro requires a prebuilt `out_dir` rather than attempting to run the script.

Basic usage

```rust
use doc_ox_buildtime_macros::embed;

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

// At runtime you can use the generated rust-embed struct:
// if let Some(bytes) = DemoAssets::get("client/dist/index.html") { ... }
```

See the parent repository for the DSL reference and examples: the macro runs the same Doc‑Ox DSL used by the `doc-ox` CLI.
