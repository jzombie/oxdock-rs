Here is the runtime context, in design-doc form, based on what we discussed.

## Goal

Build a Markdown-driven "notebook-like" runner for repos (README.md and other
docs) that can:

* Execute multiple "cells" in a single session.
* Allow later cells to reference outputs from earlier cells.
* Avoid depending on Python or mdBook.

## Core constraint

Environment variables are per-process.

* A child process cannot modify its parent process environment.
* So "run any command" in a subprocess cannot directly persist env var changes
  into a future cell, unless the parent explicitly applies updates.

This is the central reason you cannot make "any language can set NOTEBOOK_* and
it just persists" happen automatically without cooperation or embedding.

## Session model

A single long-lived session is owned by a host process (the runner).

The host maintains session state, for example:

* persisted environment map (only keys with prefix `NOTEBOOK_`)
* working directory (optional)
* cell result registry (stdout/stderr/exit code, plus optional structured data)

Cells are executed sequentially (or with a dependency graph later if desired).

## How state can persist across cells

### A) Host-managed state (recommended)

The host persists state and injects it into each cell run:

* Before executing a cell, the host passes the current persisted env into the
  child process (and sets cwd, etc.).
* After executing a cell, the host captures outputs and updates its registry.

This works for any language because it only relies on process spawning and
capturing output.

### B) In-process multi-language execution (hard)

A truly single process that runs multiple languages inside itself would require
embedding each runtime as a library so they can mutate shared host state.
This is possible for some languages, but not generally for "any language" and
not simple for compiling arbitrary Rust source.

## Why "diff env after cell" does not work generically

If a cell runs as a subprocess:

* Any env changes are confined to that subprocess.
* The parent cannot reliably query "what env vars changed inside the child"
  after it exits.

A wrapper can only diff env if the env changes happen in the wrapper process
itself (for example, shell builtins in a long-lived shell process).

## Making env updates possible anyway

If you still want cells to "set NOTEBOOK_* for future cells," you need an
explicit protocol so the child can tell the host what to do.

Two language-agnostic approaches:

1. Stdout directives

* The child prints lines like "NOTEBOOK_SET KEY=VALUE" / "NOTEBOOK_UNSET KEY"
* The host parses stdout/stderr and applies updates to its persisted env map.

2. Helper command + IPC

* Provide a small helper executable that the child calls to request updates.
* The helper talks to the host over a pipe/socket created for the session.
* The host updates its persisted env map.
* This avoids parsing and avoids eval-style risks.

## Output-based state (cleanest cross-language primitive)

Regardless of env, the most generic state you can capture from any cell is:

* stdout
* stderr
* exit code
* optional structured payload (if the cell chooses to emit it)

Each cell can be named (cell id), and later cells can reference:

* `cells.<name>.stdout`
* `cells.<name>.stderr`
* `cells.<name>.code`
* `cells.<name>.json` (if supported)

This is runtime-agnostic and works even when env mutation does not.

## Using prior cell values in later cells

Two injection mechanisms:

1. Text substitution (template expansion)

* The runner preprocesses the cell text and replaces placeholders like
  `${{ cells.help.stdout }}` before execution.
* To stay safe and predictable across languages, support explicit encoders:

  * json (string literal or JSON-escaped)
  * b64 (base64)
  * raw (explicit opt-in)

2. Environment injection (small values only)

* For small values, the host can expose selected values as env vars.
* This is limited by OS/env size limits and is string-only, so b64/json is
  commonly required.

## Markdown marker strategy

Two non-exclusive options:

* HTML comment directives (invisible in GitHub rendering):

  * Good for defining cells and generated regions without showing them.
  * Visible only in raw file/diffs.

* Fenced code blocks with metadata in the info string:

  * The fence header is not rendered, but the code block contents are rendered.
  * If you want syntax highlighting, keep the first token as the language and
    put metadata after it separated by spaces (not commas).

## Security posture (implied by the design)

Running commands embedded in Markdown is arbitrary code execution.

A reasonable default stance:

* No shell by default (run argv directly).
* Allowlist commands (or require explicit opt-in per command).
* Avoid raw template injection unless explicitly requested.
* Provide a check mode for CI (fail if generated output is stale).

This summary should be enough to anchor a design doc section on "runtime model
and state persistence."
