<div align="center">
  <img src="assets/OxDock-logo.svg" alt="OxDock logo" width="360"/>
</div>

<div align="center">
  <a href="https://www.rust-lang.org/">
    <img src="https://img.shields.io/badge/Made%20with-Rust-black?&logo=Rust" alt="Made with Rust" />
  </a>
  <a href="https://docs.rs/oxdock">
    <img src="https://img.shields.io/docsrs/oxdock" alt="docs.rs" />
  </a>
  <a href="https://deepwiki.com/jzombie/rust-oxdock">
    <img src="https://deepwiki.com/badge.svg" alt="DeepWiki" />
  </a>
</div>

# OxDock


OxDock is a Docker-like language that is [run during compile-time](./oxdock-buildtime-macros/) of Rust programs, which embeds resources into the binary's data section, not allocating to heap when the program starts. It uses [rust-embed](https://crates.io/crates/rust-embed) under the hood, for file-like access to embedded resources.

OdDock comes in two variants, each of which are independent of the other, but share the same core:

- [oxdock-buildtime-macros](./oxdock-buildtime-macros/): Provides a Rust build-time dependency which runs OxDock scripts during the compilation of a Rust program.
- [oxdock-cli](./oxdock-cli/): Command-line interface for running OxDock scripts from the command line.

OxDock has a simple goal to provide a simple language that works the same across Mac, Linux, and Windows, including support for background processes, symlinks, and boolean conditionals (such as env and platform-based command filtering), which runs the same whether it's used as a preprocessing step in a build-time Rust macro, or as a CLI program, regardless of platform it is building on.

Every internal command is engineered to run the same way across platforms, except for the RUN command, which calls native programs.

... TODO: Show example
