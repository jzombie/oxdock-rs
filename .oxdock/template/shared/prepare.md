### Prepare during the build

`oxdock_embed!` ships artifacts inside the binary. `oxdock_prepare!` runs the same script but emits no runtime module. Use it when assets only need to exist during the build, for codegen or `include!` workflows.

```rust
use oxdock_macros::oxdock_prepare;

oxdock_prepare! {
    name: PreparedAssets,
    script: {
        MKDIR gen
        WRITE gen/out.txt generated
        ASSERT_FILE gen/out.txt generated
    },
    out_dir: "target/prebuilt_prepare",
}

fn main() {}
```

