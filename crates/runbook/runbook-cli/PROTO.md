Bash script:

```bash
echo "Hello world"
```
```text runbook code=b07df148a2411cd0df4f796e27197c4b hash=97cea63631d0411350ad2cca1f06be58
Hello world
```

Rust...

<!-- ```rust
use std::io::{self, Read};
use oxdock_fs;

fn main() {
    let ws = oxdock_fs::discover_workspace_root();
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    println!("rust saw: {}, {ws:?}", input.trim());
}
``` -->

Reverse the string...

```bash
cat | rev
```
```text runbook code=1c6cef30d976adde39217b7331807e25 hash=0b97287a51fb35eb11d0fc16f9d9f49f
dlrow olleH
```
