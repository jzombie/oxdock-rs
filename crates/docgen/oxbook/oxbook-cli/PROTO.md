Bash script:

```bash
echo "Hello world"
```
```text oxbook code=6410b3f3681c962cfd74afafeb50f0f7 hash=97cea63631d0411350ad2cca1f06be58
Hello world
```

Rust...

```rust
use std::io::{self, Read};
use oxdock_fs;

fn main() {
    let ws = oxdock_fs::discover_workspace_root();
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    println!("rust saw: {}, {ws:?}", input.trim());
}
```
```text oxbook code=3e623ac863d1834e70647c7921162163 hash=257faa683d5267fef2a073ccee188572
rust saw: Hello world, Ok(GuardedPath { root: "/home/administrator/Projects/oxdock-rs", path: "/home/administrator/Projects/oxdock-rs" })
```

Reverse the string...

```bash
cat | rev
```
```text oxbook code=7a2bb4f0bf57613369b72e103513c1a0 hash=4866c2d3979773978a952785a2c28b38
)} "sr-kcodxo/stcejorP/rotartsinimda/emoh/" :htap ,"sr-kcodxo/stcejorP/rotartsinimda/emoh/" :toor { htaPdedrauG(kO ,dlrow olleH :was tsur
```
