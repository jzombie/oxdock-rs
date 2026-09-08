# docs-gen

General-purpose doc generation engine: OxDock-driven templates, plugins, and sandboxed rendering.

> Part of the [OxDock](https://github.com/jzombie/rust-oxdock) workspace.

## Overview

`docs-gen` renders text files from OxDock templates. Each target
declares ordered stages (verbatim or expanded file fragments) filled
with per-target values over global defaults. Targets are discovered
from every crate's `.oxdock/template` directory: no registry, no
hardcoded paths, any output format.

Run it with `cargo run -p docs-gen`. It accepts two flags:

- `--config <path>`: which config file to load. Without it, the runner
  looks for `./docs-gen.json`, then `./.oxdock/docs-gen.json`, and
  errors out when neither exists.
- `--root <path>`: which workspace root to render. Without it, the
  runner walks up from the current directory to the enclosing
  repository.
## Pipeline

Each run performs the same steps, in order:

1. Load `docs-gen.json` (global values file, enabled providers,
   generated-file mapping, explicit targets, extra template roots).
2. Run enabled plugins. The `command-ref` provider renders the
   `oxdock-parser` command registry into named fragments
   (`command_index`, `command_body`); the `generated_files` mapping in
   config decides which workspace-relative files they are written to.
   Any target can then consume those files as ordinary stages, so
   generated content is shareable instead of trapped in one document.
3. Collect targets: explicit `targets` entries plus every `target.json`
   discovered under each workspace member's `.oxdock/template` tree
   (and the configured roots). Directories without `target.json`
   (`_global/`, fragment-only dirs) are not targets.
4. Render each target: resolve its owning member's cargo metadata
   (`name`, `description`, `version` flow into the template context and
   into `CRATE_*` env entries), merge values (target values beat
   provider values beat global values), and execute the pure-DSL
   driver script, which concatenates stages byte-for-byte.

All dynamic values enter the DSL as runtime `$var` bindings. There is
no compile-time interpolation of runtime data and no hand-built AST:
the driver is plain DSL text parsed with the production dispatcher.

## Targets and stages

A target is declared by a `target.json` file. The common case is two
lines: discovery fills in the rest from the target directory layout:

```json
{"name": "oxdock-core-readme", "out": "crates/oxdock-core/README.md"}
```

An empty `stages` list synthesizes: `header.tmpl` expanded when
present, then verbatim `fragments/*.md`, then expanded
`fragments/*.tmpl`, then `footer.tmpl` when present. Each crate owns
its wrapper copies; global *strings* (workspace name, URLs) stay
single-sourced in the global values file and are referenced as
`{{ $docs_global.workspace }}`.

Targets with bespoke composition skip the stage list entirely and point
at a master template instead. Order comes from `{{> path }}`
positions in the document itself:

```json
{"name": "readme", "out": "README.md", "template": ".oxdock/template/readme/output.tmpl"}
```

```text
{{> .oxdock/template/readme/fragments/header.md }}
{{> .oxdock/template/shared/embed-example.md }}
{{> .oxdock/template/readme/generated/command-reference.md }}
```

A whole-line `{{> path }}` includes that file at its position:
verbatim, unless it ends in `.tmpl`, which expands with the values
context in scope. Other lines are literal document content and expand
the same way. Only `stages`-form targets needing collection (`glob`)
or inline (`text`) composition keep an explicit list.

Available stage kinds:

- `template`: `EXPAND` the file at `path`. Strict: `{{ $docs_ctx.key }}`
  and `{{ $docs_global.key }}` resolve against the values maps,
  `{{ env:KEY }}` against the environment. Unknown keys fail the run.
- `read`: `READ` the file at `path` byte-verbatim. Documented DSL
  snippets such as `{{ env:PROJECT }}` survive untouched.
- `glob`: collect `pattern` via `GLOB` (sorted, sandbox-relative), then
  per file `EXPAND` when `expand` is true, else `READ` verbatim.
- `text`: append the inline `text` string.

Every configured `out`, `template`, `values`, and stage path is
validated through `GuardedPath` before anything executes: `..`
segments and absolute paths are rejected, and relative candidates are
anchored at the workspace root (never the process working directory),
so a target can never write outside the repository.

## License

`docs-gen` is distributed under the terms of the Apache License (Version 2.0).
