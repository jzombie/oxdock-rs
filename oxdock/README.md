# oxdock

Facade crate for OxDock: the canonical user-facing entry point.

> Part of the [OxDock](https://github.com/jzombie/rust-oxdock) workspace.

- CLI runner: `oxdock::run()`, `oxdock::execute()`, `oxdock::Options`, ... —
  re-exported from `oxdock-cli` behind the `cli` feature (enabled by default).
  Running via `oxdock` or `oxdock-cli` executes identically.
- Build macros: `oxdock::oxdock!`, `oxdock::oxdock_embed!`,
  `oxdock::oxdock_prepare!` — re-exported from `oxdock-macros`, always available.

## Common usage

Install the binary:

```sh
cargo install --path oxdock
```

Run a script file:

```sh
oxdock ./build.oxfile
# same as: oxdock --script ./build.oxfile
```

Print help:

```sh
oxdock --help
```

Use the macros (macros-only build, no CLI):

```toml
[dependencies]
oxdock = { version = "0.7.0-alpha", default-features = false }
```

```rust,ignore
use oxdock::{oxdock, oxdock_embed};

oxdock_embed! {
    name: DemoAssets,
    script: {
        WORKDIR /
        WRITE hello.txt hi
    },
    out_dir: "prebuilt",
}
```

## License

`oxdock` is distributed under the terms of the Apache License (Version 2.0).
