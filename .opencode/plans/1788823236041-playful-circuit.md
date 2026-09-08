# docs-gen: General-Purpose Doc Generation Engine (Plan)

## 1. Full Requester Context (verbatim intent, for approver agent)

The `crates/docs-gen/` crate is intended to become a **general-purpose doc generation engine, not hardcoded via a set of rules**.

Design principles from requester:

- It should work in a **combination of layers, preferably mostly coded using the OxDock format and as little Rust as possible**.
- **Anything OxDock supports right now, use in the parser design.** Anything handed off to Rust gets strong consideration to move into the OxDock engine directly, but **do not make those engine changes now** — flag them.
- Requirements:
  1. **Output READMEs algorithmically decided**, ideally from each crate defining its own `.oxdock/template` directory with an output template to write to. That output template would contain the sections the output file would contain, constructed like:
     ```
     # {{ README_TITLE }}

     {{ SOME_SECTION }}
     ...
     ```
     Each crate's own templating system would **override a set of global overrides**. Values could be set as JSON, perhaps, OR other template files. The **main README treated the same way**. There should be a **global set of template strings providing defaults unless overridden, overrides scoped per README**.
  2. **Template system agnostic to output format.** Must not matter if outputting README files, TOML files, or source code, so long as the query matches the given GLOB and contains the output strings.
  3. **docs-gen parser no longer forcefully inserts `\n`.** Any formatting derived directly from templates themselves.
  4. This plan must include full context in great detail so approver agent knows what is being built.

Clarifications received 2026-09-08 (must respect):

- **Q: hardcoded `README_TITLE` keys / per-key files?** A: "I don't want hardcoded `README_TITLE`, etc. keys. Each key should not be its own template file either. Just flag this for future review." → **Do NOT hardcode a key registry. Do NOT make each key its own file. Leave key/section discovery mechanism as an OPEN design point for future review.**
- **Q: fully algorithmic discovery?** A: "Fully algorithmic and configurable. Don't even hardcode `.oxdock/template`, but it could be a start." → **Do NOT hardcode `.oxdock/template`, `docs/sections/*.md`, `docs/templates/crate-{header,footer}.md`, `docs/crates/<name>/*.md` paths. Make template-dir location + output mapping configurable; `.oxdock/template` may be the default convention, not a constant.**
- **Q: keep `command_ref.rs` in Rust?** A: "Keep that in Rust, but use it as a 'plugin' somehow where the docs gen library would be wired up to use it, but not hardcoded into the main docs-gen library itself." → **Extract command-ref generation out of `main.rs` hardcoding into a pluggable data-provider interface; docs-gen core stays generic.**

## 2. Current State (verified by exploration)

### 2.1 `crates/docs-gen/` layout (6 files, no subdirs)

- `src/main.rs` (200 lines): orchestrator. `main()` hardcodes 4 phases:
  - Phase 1: `command_ref::generate(docs/sections/07-command-body.md)`
  - Phase 1b: `command_ref::generate_index(docs/sections/06-command-reference.md)`
  - Phase 2: root `README.md` via `template_doc::compile()` with inline manifest `[{glob: docs/sections/*.md}, {template: docs/templates/crate-footer.md}]`, env `CRATE_NAME=OxDock`
  - Phase 3: `generate_crate_docs()` — parses `workspace.members` via `toml_edit`, checks `docs/templates/crate-{header,footer}.md`, per member parses `package.{name,description,version}` (fallback `No description provided.` / `workspace.package.version`), builds manifest `[header.tmpl, glob docs/crates/<name>/*.md, ?dependency.tmpl, footer.tmpl]`, calls `template_doc::compile(member/README.md, env CRATE_*)`.
- `src/template_doc.rs` (52 lines): generic assembler. Stages `manifest_json` to `.oxdock-staging/docs_manifest.json`, executes `oxdock!{...}` driver (see 2.2), deletes staging file.
- `src/runner.rs` (13 lines): `run(repo_root, steps, env)` → `GuardedPath::new_root` + `run_steps_with_context_result_with_io`.
- `src/command_ref.rs` (215 lines): pure `String` renderers from `oxdock-parser` `all_metadata()` + `all_structural_metadata()` (`### NAME`, Syntax, Args/Flags tables, Output, ```oxdock examples). `escape_table_cell` pipes, `index_anchor` lowercase. 5 unit tests.
- `Cargo.toml`: bin `docs-gen`, deps `anyhow, oxdock-core, oxdock-fs, oxdock-macros, oxdock-parser, serde_json, toml_edit` (+ unused `indoc, line-ending`). `publish=false`.
- `README.md`: generated placeholder (no `docs/crates/docs-gen/` body exists).

### 2.2 Newline insertion (the behavior to remove)

`template_doc.rs:27-47` OxDock driver:
```
WRITE #out ""
LET $manifest = LOAD_JSON(#manifest)
FOR $idx,$node IN $manifest {
  IF kind=="template" { WITH_IO [stdout=pipe:tmpl] EXPAND $node.path; WITH_IO [stdin=pipe:tmpl] APPEND #out }
  ELSE IF kind=="glob" { LET $s=GLOB($node.pattern); FOR $f IN $s { WITH_IO [stdout=pipe:sec] READ $f; WITH_IO [stdin=pipe:sec] APPEND #out; APPEND #out "\n" } }
  ELSE { APPEND #out_str $node.text }
}
```
- `WRITE/APPEND/READ/EXPAND` are byte-verbatim (no separator).
- `template` nodes: no added newline (spacing = source trailing newline).
- `glob` nodes: **verbatim + one forced `"\n"`** (`template_doc.rs:41`). Root sections already end `\n` → blank-line separators; missing trailing newline still break-guaranteed. No dedup / no CRLF normalize.
- `command_ref.rs` separately hardcodes `\n\n` paragraph breaks + trailing `push('\n')`.

### 2.3 OxDock engine capabilities available (not yet used by docs-gen)

- Grammar: `crates/oxdock-parser/src/dsl.pest`; registry: `commands.rs:declare_commands!` → `all_metadata()`.
- Leaf: `WORKDIR, WORKSPACE, ENV, INHERIT_ENV, ECHO, RUN, COPY, COPY_GIT, SYMLINK, MKDIR, LS, CWD, READ, READ_LINE, WRITE, APPEND, EXPAND [path] [K=V...], ASSERT_*, HASH_SHA256, EXIT, SLEEP`.
- Structural: `WITH_IO [stdin|stdout|stderr=pipe:N]`, `FOR $x / $k,$v IN expr`, `IF/ELSE IF/ELSE`, `LET $v = expr | LET $t = ASYNC`, `ASYNC/AWAIT/CANCEL/TIMEOUT`, guards `[env:K] [eq/neq] [unix|windows|linux|macos] [bool/not/any/all]`, comments.
- Exprs: `$var`, `$a.b.0` keypaths, `[...]`, `{k:v}`, `==/!=/&&/||/!`, `GLOB("*.md")` (quoted, sorted, sandbox-relative, empty on no-match), `LOAD_JSON(path)`, `LOAD_TOML(path)`.
- Templating (`exec/args.rs:335-453`): `{{ $var }}`, `{{ $map.key }}`, `{{ env:KEY }}`, `{{ NAME }}` (EXPAND override else var); `expand_string` missing→empty; `EXPAND` handler missing→error; escapes `\{{`; `WRITE` interpolates immediately (so `\{{` needed when writing a template for later `EXPAND`).
- File output (`exec/handlers.rs`): `WRITE/APPEND path contents?` (missing contents = stdin pipe); `READ→stdout`; `EXPAND→stdout`; capture via `WITH_IO` pipes.
- docs-gen today uses only: `WRITE/READ/APPEND/EXPAND(no K=V)/ECHO/LET/FOR/IF/WITH_IO/GLOB/LOAD_JSON/keypaths` + `#var` host interpolation + `ExecIo::insert_inherit_env`. Unused: `LOAD_TOML, ENV, WORKDIR, ASSERT_*, guards, ASYNC, COPY, etc.`

### 2.4 Current content conventions (all hardcoded, to be replaced)

- No `.oxdock/` dirs exist. Staging `.oxdock-staging/` transient only.
- Globals: `docs/templates/crate-header.md` (`# {{ env:CRATE_NAME }}` + description + workspace link), `docs/templates/crate-footer.md` (`## License ... {{ env:CRATE_NAME }}`; also appended to root README).
- Root bodies: `docs/sections/*.md` (15 files `00-header.md`…`14-coverage.md`, incl. generated `06/07`), glob-sorted order = README order.
- Per-crate bodies: `docs/crates/<CRATE_NAME>/*.md` verbatim (strict `EXPAND` would reject `{{ env:PROJECT }}` doc examples — see `main.rs:82-87`); only `docs/crates/oxdock/dependency.tmpl` expanded (pins `CRATE_VERSION`).
- No per-crate header/footer overrides — variation is env-data-only.
- Drift symptoms: duplicated `# oxdock-core` H1, `oxdock-cli` duplicated bullets, orphan `crates/oxdock/README.md` (name collision with top-level `oxdock/`).

## 3. Target Architecture

### Layer 0 — Rust core (minimal, generic, format-agnostic)

New `crates/docs-gen/src/` responsibilities (no hardcoded template paths, keys, newlines, or command-ref phases):

- `lib.rs` (new, core becomes library + thin `main.rs` binary): exposes `DocsGenConfig`, `DataProvider` trait (plugin interface), `discover_targets()`, `render_target()`.
- `runner.rs` (keep, tiny): unchanged OxDock executor.
- `config.rs` (new): loads config (e.g. `docs-gen.json` / `DocsGen.toml` at repo root, path passed via CLI arg `--config`, default discovery: `./docs-gen.json` → `./.oxdock/docs-gen.json`): fields `template_roots: [String]` (glob patterns for template dirs, default `[".oxdock/template", "docs/templates"]` as *default value, not constant*), `output_mapping`, `global_values`, `strict_expand: bool`.
- `discovery.rs` (new, or OxDock-driven — prefer OxDock): resolve `template_roots` globs → list of target dirs. Each target dir declares its own outputs (see Layer 1). No `workspace.members` hardcoding; workspace enumeration becomes one optional `DataProvider`.
- `providers.rs` (new): `trait DataProvider { fn name(&self)->&str; fn values(&self)-> serde_json::Value; fn fragments(&self)-> Vec<Fragment>; }`. Implementations: `CargoMetadataProvider` (moved `parse_cargo_metadata`/`parse_workspace_members`/`parse_workspace_version` here), `CommandRefProvider` (moved `command_ref.rs` here, output as named fragments e.g. `command_index`, `command_body`, NOT written to fixed `docs/sections/06/07` paths — caller maps them), `JsonFileProvider`, `TomlFileProvider`. `main.rs` wires providers via config list; core never imports `command_ref` directly. **No manual `StepKind` construction (rescinded per review 2026-09-08): providers only materialize values files (`values.json`/`fragments/`) and/or `ExecIo` env entries; all merge/override logic executes in pure DSL via `LET $ctx = LOAD_JSON(...)` + `{{ $ctx.key }}` keypaths + `FOR $k,$v IN $map`.**
- Delete from core: inline manifests in `main.rs:25-28,88-97`, `generate_crate_docs()` hardcoding, `find_repo_root()` → use `GuardedPath::new_root` + CLI `--root` (keep helper only as fallback). **Add `guard.rs` (or in `discovery.rs`): every configured `out` path + every `template`/`values`/`fragments` input resolved through `GuardedPath::new(repo_root, candidate)`; reject absolute paths escaping root and `..` traversal before any `WRITE`/`READ` step is built. `WRITE` targets are pre-validated `GuardedPath`s; the DSL driver receives only validated `$out` `$var` bindings.**
- Delete from core: inline manifests in `main.rs:25-28,88-97`, `generate_crate_docs()` hardcoding, `find_repo_root()` → use `GuardedPath::new_root` + CLI `--root` (keep helper only as fallback).
- Delete from core: inline manifests in `main.rs:25-28,88-97`, `generate_crate_docs()` hardcoding, `find_repo_root()` → use `GuardedPath::new_root` + CLI `--root` (keep helper only as fallback).

### Layer 1 — OxDock templates (the bulk of logic)

Per-output-target directory (convention default `.oxdock/template/`, configurable):

- `output.tmpl` (name configurable; e.g. `README.md.tmpl`, `Cargo.toml.tmpl`, `lib.rs.tmpl`): the **only** structural definition of the output file. Contains section placeholders + literal formatting (newlines, headings, fences). Example shape (keys illustrative, NOT a hardcoded registry):
  ```
  # {{ title }}

  {{ body }}
  ```
- `values.json` (optional) and/or `values.toml` (via `LOAD_TOML`): per-target override values scoping over globals. Global defaults file (e.g. `<template_root>/_global/values.json`, path from config) provides fallback keys. Resolution order in OxDock: `target values > global values > provider values > env`.
- `fragments/` (optional dir): `*.md` / `*.toml` / `*.rs` partials collected via `GLOB(pattern)` where pattern comes from config or from within `output.tmpl` context — output-format-agnostic (glob + read/expand + append works for any text).
- Main README = same mechanism: repo-root template dir (e.g. `./.oxdock/template.readme/` or `./docs/.oxdock-template/` — resolved via config, not special-cased in Rust). No Phase 2 vs Phase 3 distinction in code.

Key resolution (pure DSL, AMENDED 2026-09-08 — Rust `StepKind` directive rescinded): templates reference merged context via native keypaths `{{ $ctx.key }}`, `{{ $ctx.nested.0 }}`, plus `{{ env:KEY }}` and `FOR $k,$v IN $map` iteration. `LOAD_JSON` binds structured objects directly to script vars (`LET $ctx = LOAD_JSON($path)`); `expand_string` resolves keypaths against `ExecState` at runtime — no manual AST building, driver executes as pure DSL text via `parse_script` or `oxdock!` with `$var` only. **No Rust key registry** (per requester: flag key-naming/validation for future review — see §5 Open Questions). Override precedence emulated in DSL (target > global > provider > env) via load-order + `IF`/`FOR` merging, NOT via `EXPAND KEY=` unpacking. Strict vs lenient missing-key: use `EXPAND` (strict, error) for final output render; use lenient concat for verbatim doc-example bodies (today's `READ`-vs-`EXPAND` split preserved as a per-fragment `expand: true|false` flag in config/front-matter, not a hardcoded `.tmpl`-vs-`.md` rule — though `.tmpl`=expand / `.md`=verbatim may remain the default convention).

### Layer 2 — OxDock driver script (replaces `template_doc.rs` manifest loop) — AMENDED per code review 2026-09-08 (pure DSL)

> Amendments (no engine changes, no Rust AST hacking):
> - (a) No `#var` macro interpolation for runtime data. `#var` is `oxdock_macros` compile-time only. Dynamic paths/values from `docs-gen.json`/`target.json` enter as runtime `$var` script variables (via `ExecIo`/`ExecState` + `LET $x = ...`); driver executes as pure DSL text via `parse_script` or `oxdock!`.
> - (b) No Rust-side `StepKind::Expand` construction (prior directive rescinded per requester: "no way... it should work purely via the DSL, not hacking stepkind"). Context loading uses `LET $ctx = LOAD_JSON($path)`; templates resolve `{{ $ctx.key }}` via `expand_string` keypaths; map iteration via `FOR $k,$v IN $map`. Override precedence (target > global > provider) is emulated in-DSL via load order + `IF`/`FOR`, never via AST overrides.
> - (c) All `out` paths from config/`target.json` instantiated via `GuardedPath::new(repo_root, candidate)` in Rust before execution (sandbox containment; reject traversal outside workspace root); DSL receives only pre-validated `$out` bindings.

Replace `WRITE "" + LOAD_JSON(manifest) + FOR + IF template/glob + APPEND "\n"` with a pure-DSL renderer driven by target context. Correct pattern (illustrative `$var`-only DSL, final syntax in implementation):

```
LET $global = LOAD_JSON($global_values_path)
LET $local = LOAD_JSON($target_values_path)
FOR $k, $v IN $local {
  ECHO "{{ $k }}={{ $v }}"
}
WITH_IO [stdout=pipe:rendered] EXPAND $output_tmpl
WITH_IO [stdin=pipe:rendered] WRITE $out
```

Where `$global_values_path`, `$target_values_path`, `$output_tmpl`, `$out`, `$frags_pattern` are runtime `$var` bindings (NOT `#var`); `output.tmpl` itself contains `{{ $ctx.title }}`-style keypaths and `{{ env:KEY }}` refs resolved natively by `expand_string`. Fragment assembly (verbatim `READ` vs expandable `EXPAND`) branches on per-fragment `expand` flag with `IF/ELSE`, iterating `GLOB($pattern)` results with `FOR $f IN $frags`.

- Fragment globs resolved via `GLOB($pattern-var)` (quoted-pattern rule still holds; pattern string itself may come from a `$var`). Verbatim doc-example fragments use `READ→APPEND`; expandable fragments use `EXPAND $path` with context already in `$var` scope — never via map-unpacking or Rust AST.
- **No `APPEND $out "\n"` anywhere.** Formatting comes solely from `output.tmpl` bytes + fragment bytes. Verbatim `READ` fragments concatenated exactly; `EXPAND` fragments preserve source newlines.
- `ECHO` progress lines kept (optional `--quiet`).

### What stays in Rust vs moves to OxDock (per "as little Rust as possible", no engine changes now) — AMENDED 2026-09-08 (pure DSL)

- Rust: CLI arg parsing, config file load, target discovery filesystem walk (if `GLOB` insufficient), `DataProvider` trait + 2 impls (cargo-metadata, command-ref) materializing values/fragment files, `GuardedPath::new` sandbox validation of every configured `out`/input path + runtime `$var` injection via `ExecIo` (zero `#var` for dynamic data, zero manual `StepKind` construction), staging dir management, invoking `run_steps_with_context_result_with_io` on pure-DSL scripts (`parse_script`/`oxdock!`). `command_ref.rs` renderers move unchanged behind `CommandRefProvider`.
- OxDock (pure DSL): all ordering, conditionals, iteration (`FOR $k,$v IN $map`), `GLOB($var)`, `LOAD_JSON/LOAD_TOML` context loading (`LET $ctx = LOAD_JSON($path)`), `READ/WRITE/APPEND/EXPAND $path` with `{{ $ctx.key }}` keypath resolution via `expand_string`, value-override precedence, final byte layout (no forced newlines).
- Flagged for future engine work (DO NOT implement now): generic `{{ }}` key-validation/missing-key policy switch, recursive `EXPAND` includes, JSON-merge helper (currently emulated in-DSL via load order + `IF`/`FOR`), plugin `DataProvider` invocation from DSL — candidates to move into OxDock engine later; DSL-side workarounds must be documented inline when encountered.

## 4. Implementation Steps

1. **Refactor to library + config (Rust, small):** add `lib.rs`, `config.rs` (`template_roots`, `outputs: [{template, out, values, fragments, expand}]`, `providers: [String]`, `global_values` path); `main.rs` becomes arg-parse + `discover_targets()` + per-target `render_target()`. Keep old behavior behind default config so `cargo run -p docs-gen` reproduces current READMEs byte-for-byte (minus planned newline change — gate newline removal behind config flag for diff review).
2. **Plugin-ize providers (Rust, move-only):** extract `parse_*` → `CargoMetadataProvider`, `command_ref.rs` → `CommandRefProvider` impl `DataProvider`; remove `command_ref::generate*` file writes from `main()`; generated `06/07` become fragments consumed via template mapping, not fixed output paths. Add `JsonFile/TomlFile` providers (thin `LOAD_JSON/LOAD_TOML` wrappers for global/per-target values).
3. **Generic OxDock renderer (pure DSL, replaces `template_doc.rs`):** implement Layer-2 driver as pure DSL text (`parse_script`/`oxdock!`, `$var` only — zero `#var` for runtime data, zero manual `StepKind` construction) using `LOAD_JSON/LOAD_TOML` context vars + `{{ $ctx.key }}` keypaths + `FOR $k,$v` merging, `GLOB($var)`, `READ`, `EXPAND $path`, `WRITE/APPEND`, `WITH_IO`, `FOR/IF/LET`; delete forced `APPEND "\n"`; support per-fragment `expand:true|false`; support any text output (no `.md` assumptions; output path + template path arbitrary `$var` strings, both `GuardedPath`-validated in Rust before execution).
4. **Conventional template dirs (content, not code):** create default `.oxdock/template/` (or `docs/.oxdock-template/` if `.oxdock/` collides with tooling) for root README + one pilot crate (e.g. `crates/docs-gen/.oxdock/template/`), with `output.tmpl + values.json + fragments/`; add `_global/values.json` defaults (`license_text`, `workspace_link`, etc. — names illustrative, not registry). Port `crate-header/footer.md` content into these. Keep legacy `docs/sections + docs/templates + docs/crates` working via compat config until migration complete.
5. **Algorithmic discovery (config-driven):** `template_roots` globs discover all target dirs; each dir's `output.tmpl` front-matter or sibling `target.json` declares `out` path (relative to repo root) + fragment globs. Root README is just another entry. Remove `workspace.members` enumeration as the driver (keep as optional provider for `CRATE_*` values).
6. **Verification:** `cargo run -p docs-gen` regenerates all `README.md` (+ pilot TOML/code outputs proving format-agnosticism); `git diff` reviewed for unintended whitespace (expected: blank-line collapses where forced `\n` used to fire); `cargo test --workspace --tests` green; `cargo clippy --workspace` clean (no `_`-prefix params, no blanket allows per workspace guardrails); add `crates/docs-gen/tests/` golden tests: (a) no-forced-newline (template without trailing newline → output without trailing newline), (b) override scoping (per-target beats global), (c) format-agnostic (same engine emits `.md` + `.toml` + `.rs`), (d) plugin isolation (core builds with `CommandRefProvider` disabled).

## 5. Open Questions (flagged for future review, DO NOT decide in implementation)

1. **Section/key mechanism:** requester explicitly rejected hardcoded keys (`README_TITLE`, `SOME_SECTION`) and per-key-file layouts. Future review needed: how does `output.tmpl` declare/discover available sections? (front-matter? fragment-glob naming? provider schema?) Implementation must keep this generic — no key allowlist in Rust or OxDock.
2. **Values format:** JSON vs TOML vs template-files for overrides — support `LOAD_JSON` + `LOAD_TOML` both; whether per-key files ever needed remains open.
3. **Template-dir location:** `.oxdock/template` is a starting convention only; final path(s) configurable. Also resolve potential collision with OxDock tooling's `.oxdock*` internal artifacts (`is_internal_artifact` filter).
4. **Engine promotions:** strict/lenient `EXPAND` policy flag, JSON-merge operator, recursive includes, provider invocation from DSL — candidates to move into OxDock engine later; OxDock-side workarounds must be documented inline when encountered.
5. **Legacy content migration:** `docs/sections/`, `docs/templates/`, `docs/crates/`, generated `06/07` section files, stale `crates/oxdock/README.md` collision — migrate/delete plan needs approver sign-off (compat config duration).

## 6. Verification

- `cargo run -p docs-gen -- --config <cfg>` → all targets render; exit 0; `.oxdock-staging/` cleaned.
- Golden tests in `crates/docs-gen/tests/` cover §4.6 a–d.
- Existing suites: `cargo test --workspace --tests`; `cargo clippy --workspace -- -D warnings` (respect `clippy.toml` denies; localized allows only); formatting via templates (no Rust-inserted `\n` — grep `APPEND.*\\n` in `template_doc.rs`/renderer returns empty).
- Format-agnosticism demo: single run emits at least one `.md`, one `.toml`, one source file from same engine path.
- Approver checks: no hardcoded output paths/keys/newlines in `crates/docs-gen/src/`; `command_ref` only referenced via provider wiring; template-dir paths only as config defaults; **no `#var` tokens bound to runtime data (grep `oxdock!` blocks for `#` — only compile-time constants allowed; dynamic DSL uses `$var`); no manual `StepKind` construction (driver is pure DSL text); every `out` path passes `GuardedPath::new` sandbox check (add negative test: `../escape` rejected).**

## 7. Critical Files

- To modify: `crates/docs-gen/src/main.rs`, `crates/docs-gen/src/template_doc.rs` (replace), `crates/docs-gen/src/runner.rs` (minor), `crates/docs-gen/Cargo.toml` (lib target + deps), new `crates/docs-gen/src/{lib,config,discovery,providers}.rs`, new `crates/docs-gen/tests/*.rs`, new template dirs + `docs-gen.json`/equivalent config.
- To move (not rewrite): `crates/docs-gen/src/command_ref.rs` → provider impl.
- To read: `crates/oxdock-parser/src/dsl.pest`, `crates/oxdock-parser/src/commands.rs` (registry + `EXPAND`), `crates/oxdock-core/src/exec/{args.rs (expand_string keypaths, LOAD_JSON→Value::Map, FOR map iteration),handlers.rs}` (templating/GLOB/IO semantics), `crates/sys/oxdock-fs/src/` (`GuardedPath::new` sandbox validation), `oxdock-macros/src/lib.rs` (`oxdock!` `#var` = compile-time only; dynamic DSL uses `$var` via `parse_script`), `docs/templates/crate-{header,footer}.md`, `docs/crates/oxdock/dependency.tmpl`, `docs/sections/00-header.md` (root shape), `Cargo.toml` (workspace members/version).
