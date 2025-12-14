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

OxDock comes in two variants, each of which are independent of the other, but share the same core:

- [oxdock-buildtime-macros](./oxdock-buildtime-macros/): Provides a Rust build-time dependency which runs OxDock scripts during the compilation of a Rust program.
- [oxdock-cli](./oxdock-cli/): Command-line interface for running OxDock scripts from the command line.

OxDock has a simple goal to provide a simple language that works the same across Mac, Linux, and Windows, including support for background processes, symlinks, and boolean conditionals (such as env and platform-based command filtering), which runs the same whether it's used as a preprocessing step in a build-time Rust macro, or as a CLI program, regardless of platform it is building on.

Every internal command is engineered to run the same way across platforms, except for the RUN command, which calls native programs.

... TODO: Show example

## Path Separators

- **Cross-platform behavior:** Paths in OxDock scripts are treated as filesystem paths and are resolved using Rust's `Path`/`PathBuf` APIs. That means you can use either `/`-separated paths or `./`-prefixed relative paths in scripts and they will be interpreted correctly on Windows, macOS, and Linux.

- **Path separator preference / requirement:** For consistency and portability, OxDock scripts should use the forward slash (`/`) as the path separator in script source. While the runtime resolves paths using platform APIs and will accept platform-specific absolute paths, using `/` in scripts (even on Windows) avoids needing to escape backslashes (`\`) and matches Docker-style examples. If you must reference a native Windows absolute path, prefer the `C:/path/to` form or escape backslashes carefully.

- **Relative paths:** A leading `./` indicates a path relative to the current DSL working directory (the same semantics used by Docker). For example: `COPY ./src ./out` or `SYMLINK ./dir ./dir-link` will work on all platforms.

- **Absolute paths:** Use platform-appropriate absolute paths (e.g., `/usr/bin` on Unix-like systems, `C:\path\to` on Windows). OxDock will use the host OS path semantics when resolving absolute paths.

- **Symlinks and Windows:** Creating symlinks on Windows may require elevated permissions on some older OS versions; where symlinks are not available the CLI falls back to copying directory contents so scripts remain functional across platforms.

- **Globbing & shell expansion:** OxDock does not implicitly perform shell globbing or shell-side expansion for file arguments — when you need shell semantics use `RUN` with the platform shell, or add explicit DSL commands that accept wildcards if you want portable behavior.
