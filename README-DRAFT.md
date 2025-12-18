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
  <a href="https://github.com/jzombie/oxdock-rs/actions/workflows/rust-tests.yml?query=branch%3Amain+event%3Apush">
    <img src="https://img.shields.io/github/actions/workflow/status/jzombie/oxdock-rs/rust-tests.yml?branch=main&label=Miri&logo=github" alt="Miri status" />
  </a>
  <a href="https://deepwiki.com/jzombie/rust-oxdock">
    <img src="https://deepwiki.com/badge.svg" alt="DeepWiki" />
  </a>
</div>

# OxDock


OxDock is a Docker-inspired language that is [run during compile-time](./oxdock-buildtime-macros/) of Rust programs, which embeds resources into the binary's data section, not allocating to heap when the program starts. It uses [rust-embed](https://crates.io/crates/rust-embed) under the hood, for file-like access to embedded resources.

OxDock comes in two variants, each of which are independent of the other, but share the same core:

- [oxdock-buildtime-macros](./oxdock-buildtime-macros/): Provides a Rust build-time dependency which runs OxDock scripts during the compilation of a Rust program.
- [oxdock-cli](./oxdock-cli/): Command-line interface for running OxDock scripts from the command line.

OxDock has a simple goal to provide a simple language that works the same across Mac, Linux, and Windows, including support for background processes, symlinks, and boolean conditionals (such as env and platform-based command filtering), which runs the same whether it's used as a preprocessing step in a build-time Rust macro, or as a CLI program, regardless of platform it is building on.

Every internal command is engineered to run the same way across platforms, except for the RUN command, which calls native programs.

... TODO: Mention that OxDock adds no additional runtime dependencies if used as a preprocessor.  
... TODO: Show example

## Testing

Testing is performed across Linux, Mac, and Windows environments, and UB (Undefined Behavior) testing is handled by [Miri](https://github.com/rust-lang/miri).

There is strong prioritzation in keeping unit and integration tests compatible with Miri, because doing so also encourages clean separation of process and filesystem modeling from direct OS calls, avoiding scattered filesystem and process usage throughout the codebase.

## Path Separators

- **Cross-platform behavior:** Paths in OxDock scripts are treated as filesystem paths and are resolved using Rust's `Path`/`PathBuf` APIs. That means you can use either `/`-separated paths or `./`-prefixed relative paths in scripts and they will be interpreted correctly on Windows, macOS, and Linux.

- **Path separator preference / requirement:** For consistency and portability, OxDock scripts should use the forward slash (`/`) as the path separator in script source. While the runtime resolves paths using platform APIs and will accept platform-specific absolute paths, using `/` in scripts (even on Windows) avoids needing to escape backslashes (`\`) and matches Docker-style examples. If you must reference a native Windows absolute path, prefer the `C:/path/to` form or escape backslashes carefully.

- **Relative paths:** A leading `./` indicates a path relative to the current DSL working directory (the same semantics used by Docker). For example: `COPY ./src ./out` or `SYMLINK ./dir ./dir-link` will work on all platforms.

- **Absolute paths:** Use platform-appropriate absolute paths (e.g., `/usr/bin` on Unix-like systems, `C:\path\to` on Windows). OxDock will use the host OS path semantics when resolving absolute paths.

- **Symlinks and Windows:** Creating symlinks on Windows may require elevated permissions on some older OS versions; where symlinks are not available the CLI falls back to copying directory contents so scripts remain functional across platforms.

- **Globbing & shell expansion:** OxDock does not implicitly perform shell globbing or shell-side expansion for file arguments — when you need shell semantics use `RUN` with the platform shell, or add explicit DSL commands that accept wildcards if you want portable behavior.

## COPY_GIT: copy from another Git revision

OxDock supports copying files or directories out of a Git repository at a specific revision via the `COPY_GIT` instruction.

- Syntax: `COPY_GIT <rev> <src_path> <dst_path>`
  - `<rev>` is any git revision spec (branch, tag, or commit-ish) that `git` understands.
  - `<src_path>` is a path inside the repository (relative to the build context / local workspace).
  - `<dst_path>` is a path inside the current OxDock workspace where the content will be placed.

- Semantics:
  - If `<src_path>` is a file in the given revision, OxDock uses `git show <rev>:<src_path>` and writes the blob to `<dst_path>`.
  - If `<src_path>` is a tree (directory), OxDock uses `git archive --format=tar <rev> <src_path>` and extracts the tree, then copies the extracted files into `<dst_path>`.
  - All reads are performed via git plumbing — OxDock does not check out the revision into the working directory.

- Safety and containment:
  - `COPY_GIT` reads from the configured build context (the local repository path passed to the runner) and is only allowed to read within that build context. Attempts to reference absolute paths for `<src_path>` are rejected.
  - The destination `<dst_path>` is validated against OxDock's workspace containment rules: writes outside the allowed workspace root are rejected.

- Requirements and caveats:
  - The implementation uses the `git` CLI and `tar` for archive extraction; these must be available on the host running OxDock (CI runners typically have them). If you need a zero-dependency fallback, we can implement a pure-git fallback using `git ls-tree` + `git show`.
  - Symlinks stored in the git tree will be created by `tar` when extracting; on Windows this may require appropriate permissions.
  - Errors from `git` (missing ref, missing path at ref) are surfaced to the script runner.

- Example:
```

## Workspaces & Filesystem (draft)

- **How workspaces are created:** OxDock materializes a clean workspace by using Git's archival capabilities (the CLI runs `git archive`/`tar` under the hood). That produces a copy of the repository content at HEAD without the `.git` metadata; the result is very fast and lightweight compared to full checkouts. Treat this materialized tree as a scratchpad surface for experimentation: you can run scripts inside it, create or modify files, and prepare assets for publishing without affecting your main source tree or requiring `--allow-dirty` workflows.

- **Typical usage pattern:** the materialized workspace is intended for short-lived build/test iterations — run scripts against it, inspect outputs, and discard when done. Because it is just a filesystem snapshot it is safe to run multiple concurrent experiments without changing the original repo.

- **Filesystem gating via `oxdock-fs`:** all filesystem operations in the runtime are routed through the crate-internal `oxdock-fs` abstraction. That module centralizes path resolution, canonicalization and access checks so reads and writes can be validated against the allowed workspace root and build context.

- **What `oxdock-fs` protects you from:** the guardrails are pragmatic — they prevent common mistakes such as accidentally writing outside the materialized workspace or reading files from arbitrary absolute paths. However, they are not a full sandbox: a determined process or script can still create destructive actions (e.g., invoking native `RUN` commands that modify external state). If you require strict isolation, run OxDock inside a container or VM.

- **Performance:** routing via `oxdock-fs` adds negligible overhead for typical workloads. The module focuses on correctness and containment with minimal runtime cost so interactive iteration remains fast.

> Note: this is a draft—feel free to tweak phrasing or add pointers on how to enable stricter isolation (examples: running inside Docker, enabling CI-only checks, or using ephemeral VMs).

# copy a single file from the `release` branch
COPY_GIT release path/to/config.toml app/config/config.toml

# copy a directory from a specific commit
COPY_GIT 7a2b1c4 src/lib/my_assets public/assets
```

## Guard blocks and multi-line conditions

Guards can now span multiple lines and wrap entire blocks of commands. This makes it easy to express platform or environment specific logic without repeating the same `[]` prefix on every line.

```text
[ env:PROFILE=release,
  linux
]
WRITE linux-release.txt generated

[platform:windows] {
    WRITE win.txt hi
    RUN powershell -Command Write-Host "windows!"
}
```

- Square brackets may span multiple lines; commas express `AND`, pipes express `OR`.
- Attaching a `{ ... }` block to a guard applies the guard to every enclosed command.
- Guard-only lines without a block apply to the next command, preserving the existing syntax.
- Commands inside `{ ... }` run inside a scoped environment: changes to `WORKDIR`, `WORKSPACE`, or `ENV` revert once the block exits so temporary setup does not leak outward.
# OxDock

[![Coverage Status](https://coveralls.io/repos/github/jzombie/oxdock-rs/badge.svg?branch=main)](https://coveralls.io/github/jzombie/oxdock-rs?branch=main)
[![Miri Coverage](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fjzombie%2Foxdock-rs%2Fbadges%2Fmiri-coverage.json)](#miri-coverage)

OxDock is a Rust workspace that explores guarded filesystem access, process isolation, and deterministic tooling. The project is still evolving and its README will grow as more components are documented.

## Coverage reporting

### LLVM line coverage (cargo-llvm-cov)

The `coverage (cargo-llvm-cov)` GitHub Actions job installs [`cargo-llvm-cov`](https://github.com/taiki-e/cargo-llvm-cov) and publishes [`lcov`](https://github.com/linux-test-project/lcov) data to Coveralls. Once the repository is enabled on Coveralls, pushes and pull requests to `main` automatically update the badge above.

To reproduce the report locally (requires the nightly LLVM tools component):

```bash
cargo install cargo-llvm-cov
rustup component add llvm-tools-preview
cargo llvm-cov --workspace --all-features --lcov --output-path lcov.info
```

### Miri coverage

The CI `miri` job monitors how many workspace unit tests can run under [`cargo miri`](https://github.com/rust-lang/miri). On pushes to `main`, the job publishes a badge description (`badges/miri-coverage.json` on the `badges` branch) that backs the Miri coverage badge above.

To keep the badge grounded in real coverage reporting, the workflow multiplies two signals:

1. **Runnable test ratio:** how many workspace tests are runnable under Miri vs. the total (`cargo miri test -- --list`).
2. **LLVM line coverage baseline:** the percent reported by `cargo llvm-cov --summary-only` (the same value sent to Coveralls).

The badge therefore shows an approximate “effective Miri coverage” (baseline coverage × runnable ratio), which can never exceed the standard coverage percentage but gives a tangible sense of how much of the tested surface area is validated under the interpreter.

To test the calculation locally without waiting for CI:

```bash
cargo llvm-cov --workspace --all-features --summary-only > coverage-summary.txt
BASE_LINE_COVERAGE=$(awk '/^TOTAL/ {print $10}' coverage-summary.txt | tr -d '%' | head -n1) \
  scripts/.github/miri-badge-report.sh
```

The helper emits the same badge JSON (`badges/miri-coverage.json`) and summary text used by CI, making it easy to confirm the numbers before opening a PR.

If you run new tests under Miri locally, you can sanity-check parity with CI via:

```bash
cargo +nightly miri setup
cargo +nightly miri test --workspace --all-features --lib --tests
```
