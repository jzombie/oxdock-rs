# oxdock

Facade crate for OxDock: CLI runner and build macros.

> Part of the [OxDock](https://github.com/jzombie/rust-oxdock) workspace.
- CLI runner: `oxdock::run()`, `oxdock::execute()`, `oxdock::Options`, ... —
  re-exported from `oxdock-cli` behind the `cli` feature (enabled by default).
  Running via `oxdock` or `oxdock-cli` executes identically.
- Build macros: `oxdock::oxdock!`, `oxdock::oxdock_embed!`,
  `oxdock::oxdock_prepare!` — re-exported from `oxdock-macros`, always available.

## Common usage

Install the binary from the registry:

```sh
cargo install oxdock
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

```sh
cargo add oxdock --no-default-features
```

Or pin the version in `Cargo.toml`:

```toml
[dependencies]
oxdock = { version = "0.7.0-alpha", default-features = false }
```

```rust
use oxdock::oxdock_embed;

oxdock_embed! {
    name: DemoAssets,
    script: {
        WORKDIR /
        WRITE hello.txt hi
    },
    out_dir: "prebuilt",
}

fn main() {
    let file = DemoAssets::get("hello.txt").expect("hello.txt must be embedded");
    assert_eq!(file.data.as_ref(), b"hi");
}
```

## License

`oxdock` is distributed under the terms of the Apache License (Version 2.0).
