```rust
use oxdock_macros::oxdock_embed;

oxdock_embed! {
    // Embedded resources are mapped to `HelloAssets::get(resource)`
    name: HelloAssets,
    script: {
        ENV PROJECT=OxDock
        MKDIR dist
        WRITE dist/hello.txt Built with {{ env:PROJECT }}
        ASSERT_FILE dist/hello.txt Built with {{ env:PROJECT }}
    },
    // Generated assets land under target/, keeping the source tree clean
    out_dir: "target/prebuilt",
}

fn main() {
    // Verify we can read the resource we just created
    let file = HelloAssets::get("dist/hello.txt").expect("dist/hello.txt must be embedded");
    assert_eq!(file.data.as_ref(), b"Built with OxDock");
}
```

