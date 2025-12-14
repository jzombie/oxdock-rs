# OxDock

[![made-with-rust][rust-logo]][rust-src-page]
[![rust-docs][rust-docs-badge]][rust-docs-page]
[![Ask DeepWiki][deepwiki-badge]][deepwiki-page]

OxDock is a Docker-like language that is [run during compile-time](./oxdock-buildtime-macros/) that embeds resources into the binary's data section, not allocating to heap when the program starts. It uses [rust-embed](https://crates.io/crates/rust-embed) under the hood, for file-like access to embedded resources.

... TODO: Show example

[rust-src-page]: https://www.rust-lang.org/
[rust-logo]: https://img.shields.io/badge/Made%20with-Rust-black?&logo=Rust

[rust-docs-page]: https://docs.rs/oxdock
[rust-docs-badge]: https://img.shields.io/docsrs/oxdock

[deepwiki-page]: https://deepwiki.com/jzombie/rust-oxdock
[deepwiki-badge]: https://deepwiki.com/badge.svg
