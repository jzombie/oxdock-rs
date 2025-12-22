```passthru
testing 1 2 3
```

```rust
use std::io::{self, Read};

fn main() {
	let mut input = String::new();
	io::stdin().read_to_string(&mut input).unwrap();
	println!("rust saw: {}", input.trim());
}
```
