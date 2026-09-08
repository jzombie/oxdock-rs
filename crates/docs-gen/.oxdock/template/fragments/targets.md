## Targets and stages

A target is declared by a `target.json` file. The common case is two
lines — discovery fills in the rest from the target directory's own
layout:

```json
{"name": "oxdock-core-readme", "out": "crates/oxdock-core/README.md"}
```

An empty `stages` list synthesizes: `header.tmpl` expanded when
present, then verbatim `fragments/*.md`, then expanded
`fragments/*.tmpl`, then `footer.tmpl` when present. Each crate owns
its wrapper copies; global *strings* (workspace name, URLs) stay
single-sourced in the global values file and are referenced as
`{{ $docs_global.workspace }}`. Targets needing bespoke composition
declare full `stages` and discovery leaves them untouched.

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

