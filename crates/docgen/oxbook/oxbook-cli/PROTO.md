Bash script:

```bash
echo "Hello world"
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

    eprintln!("test stderr");
}
```

Reverse the string...

```bash
cat | rev
```
