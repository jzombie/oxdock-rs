Scripts run during `rustc`, and their artifacts ship inside the binary with zero heap allocation:

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

For each artifact the macro emits a constant backed by `include_bytes!`, which bakes the file bytes into read-only binary data during compilation. At runtime `get()` scans a static table and returns a borrowed slice, so there are no file reads and no heap allocation. The support types only need `alloc::borrow::Cow` and core iterators, which is why it works in `no_std`.

