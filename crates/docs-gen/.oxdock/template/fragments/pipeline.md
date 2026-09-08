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
   (and the configured roots). Directories without `target.json` —
   `_global/`, fragment-only dirs — are not targets.
4. Render each target: resolve its owning member's cargo metadata
   (`name`, `description`, `version` flow into the template context and
   into `CRATE_*` env entries), merge values (target values beat
   provider values beat global values), and execute the pure-DSL
   driver script, which concatenates stages byte-for-byte.

All dynamic values enter the DSL as runtime `$var` bindings. There is
no compile-time interpolation of runtime data and no hand-built AST:
the driver is plain DSL text parsed with the production dispatcher.

