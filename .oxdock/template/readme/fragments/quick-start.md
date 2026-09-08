## Quick start

The gist is compile-time embedding — the macro runs the script during `rustc` and generates a pure-Rust struct whose assets live in the binary's data section, readable at runtime with zero heap allocation:

