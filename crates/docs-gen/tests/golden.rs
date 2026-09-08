use docs_gen::{
    DocsGenConfig, TargetSpec,
    discovery::{discover_targets, member_for_dir},
    guard::validate_rel_path,
    providers::{CommandRefProvider, DataProvider},
    template_doc,
};
use oxdock_core::ExecIo;
use oxdock_fs::{GuardedPath, PathResolver};

// ---------------------------------------------------------------------------
// Harness: an isolated workspace root with guarded file IO (no raw std::fs
// on guarded paths, per workspace guardrails).
// ---------------------------------------------------------------------------

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
struct Fixture {
    _temp: oxdock_fs::GuardedTempDir,
    root: GuardedPath,
    resolver: PathResolver,
}

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
impl Fixture {
    fn new() -> Self {
        let temp = GuardedPath::tempdir().expect("tempdir");
        let root = temp.as_guarded_path().clone();
        let resolver = PathResolver::new(root.as_path(), root.as_path()).expect("resolver");
        Self {
            _temp: temp,
            root,
            resolver,
        }
    }

    fn write(&self, rel: &str, contents: &str) {
        let path = self.root.join(rel).expect("join");
        if let Some(parent) = path.as_path().parent() {
            let parent_guard = GuardedPath::new(self.root.as_path(), parent).expect("guard parent");
            self.resolver.create_dir_all(&parent_guard).expect("mkdir");
        }
        self.resolver
            .write_file(&path, contents.as_bytes())
            .expect("write");
    }

    fn read(&self, rel: &str) -> String {
        let path = self.root.join(rel).expect("join");
        self.resolver.read_to_string(&path).expect("read")
    }

    fn repo_root(&self) -> &std::path::Path {
        self.root.as_path()
    }
}

fn target(name: &str, out: &str, stages_json: serde_json::Value) -> TargetSpec {
    let stages = serde_json::from_value(stages_json).expect("stages");
    TargetSpec {
        name: name.to_string(),
        out: out.to_string(),
        values: None,
        stages,
        member: None,
        template: None,
    }
}

// ---------------------------------------------------------------------------
// (a) No forced newlines: output bytes equal template/fragment bytes exactly.
// ---------------------------------------------------------------------------

#[test]
#[cfg_attr(
    miri,
    ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
)]
fn no_forced_newline_between_stages() {
    let fx = Fixture::new();
    // Deliberately missing trailing newlines everywhere.
    fx.write("head.tmpl", "A");
    fx.write("frags/one.md", "B");
    let out = target(
        "concat",
        "out.txt",
        serde_json::json!([
            {"kind": "template", "path": "head.tmpl"},
            {"kind": "glob", "pattern": "frags/*.md", "expand": false},
            {"kind": "text", "text": "C"},
        ]),
    );
    template_doc::render_target(fx.repo_root(), &out, None, None, ExecIo::new()).expect("render");
    assert_eq!(fx.read("out.txt"), "ABC");
}

#[test]
#[cfg_attr(
    miri,
    ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
)]
fn newlines_come_from_templates_only() {
    let fx = Fixture::new();
    fx.write("head.tmpl", "A\n");
    fx.write("frags/one.md", "B\n");
    let out = target(
        "concat",
        "out.txt",
        serde_json::json!([
            {"kind": "template", "path": "head.tmpl"},
            {"kind": "glob", "pattern": "frags/*.md", "expand": false},
        ]),
    );
    template_doc::render_target(fx.repo_root(), &out, None, None, ExecIo::new()).expect("render");
    // Exactly the source bytes — no extra blank line injected.
    assert_eq!(fx.read("out.txt"), "A\nB\n");
}

// ---------------------------------------------------------------------------
// (b) Override scoping: per-target values beat globals; globals fill gaps.
// ---------------------------------------------------------------------------

#[test]
#[cfg_attr(
    miri,
    ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
)]
fn target_values_override_globals() {
    let fx = Fixture::new();
    fx.write("global.json", r#"{"title":"global","footer":"shared"}"#);
    fx.write("values.json", r#"{"title":"local"}"#);
    fx.write(
        "output.tmpl",
        "# {{ $docs_ctx.title }}\n\n{{ $docs_ctx.footer }}\n",
    );
    let mut out = target(
        "scoped",
        "out.md",
        serde_json::json!([
            {"kind": "template", "path": "output.tmpl"},
        ]),
    );
    out.values = Some("values.json".to_string());
    template_doc::render_target(
        fx.repo_root(),
        &out,
        Some("global.json"),
        None,
        ExecIo::new(),
    )
    .expect("render");
    assert_eq!(fx.read("out.md"), "# local\n\nshared\n");
}

#[test]
#[cfg_attr(
    miri,
    ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
)]
fn provider_values_lose_to_target_values() {
    let fx = Fixture::new();
    fx.write("values.json", r#"{"title":"local"}"#);
    fx.write(
        "output.tmpl",
        "{{ $docs_ctx.title }}/{{ $docs_ctx.extra }}\n",
    );
    let mut out = target(
        "layered",
        "out.txt",
        serde_json::json!([
            {"kind": "template", "path": "output.tmpl"},
        ]),
    );
    out.values = Some("values.json".to_string());
    let provider = serde_json::json!({"title": "provider", "extra": "from-provider"});
    template_doc::render_target(fx.repo_root(), &out, None, Some(&provider), ExecIo::new())
        .expect("render");
    assert_eq!(fx.read("out.txt"), "local/from-provider\n");
}

// ---------------------------------------------------------------------------
// (c) Format agnosticism: the same engine emits Markdown, TOML, source.
// ---------------------------------------------------------------------------

#[test]
#[cfg_attr(
    miri,
    ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
)]
fn same_engine_emits_toml_and_rust() {
    let fx = Fixture::new();
    fx.write("values.json", r#"{"title":"demo"}"#);
    fx.write("out.tmpl", "title = \"{{ $docs_ctx.title }}\"\n");
    fx.write(
        "code.tmpl",
        "pub const TITLE: &str = \"{{ $docs_ctx.title }}\";\n",
    );
    let toml_target = TargetSpec {
        values: Some("values.json".to_string()),
        ..target(
            "toml",
            "out.toml",
            serde_json::json!([{"kind": "template", "path": "out.tmpl"}]),
        )
    };
    let rs_target = TargetSpec {
        values: Some("values.json".to_string()),
        ..target(
            "rust",
            "out.rs",
            serde_json::json!([{"kind": "template", "path": "code.tmpl"}]),
        )
    };
    for target in [&toml_target, &rs_target] {
        template_doc::render_target(fx.repo_root(), target, None, None, ExecIo::new())
            .expect("render");
    }
    assert_eq!(fx.read("out.toml"), "title = \"demo\"\n");
    assert_eq!(fx.read("out.rs"), "pub const TITLE: &str = \"demo\";\n");
}

#[test]
#[cfg_attr(
    miri,
    ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
)]
fn verbatim_fragments_survive_strict_expansion() {
    let fx = Fixture::new();
    // Documented DSL snippets must not trigger strict EXPAND failures.
    fx.write("frags/snippet.md", "Run `{{ env:PROJECT }}` next.\n");
    let out = target(
        "verbatim",
        "out.md",
        serde_json::json!([
            {"kind": "glob", "pattern": "frags/*.md", "expand": false},
        ]),
    );
    template_doc::render_target(fx.repo_root(), &out, None, None, ExecIo::new()).expect("render");
    assert_eq!(fx.read("out.md"), "Run `{{ env:PROJECT }}` next.\n");
}

// ---------------------------------------------------------------------------
// (d) Sandbox: traversal rejected at the config boundary.
// ---------------------------------------------------------------------------

#[test]
#[cfg_attr(
    miri,
    ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
)]
fn guard_rejects_traversal() {
    let fx = Fixture::new();
    for candidate in ["../escape.md", "a/../../escape.md", "/abs.md"] {
        assert!(
            validate_rel_path(fx.repo_root(), candidate).is_err(),
            "{candidate} must be rejected"
        );
    }
    assert!(validate_rel_path(fx.repo_root(), "docs/out.md").is_ok());
}

#[test]
#[cfg_attr(
    miri,
    ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
)]
fn render_target_rejects_escape_out() {
    let fx = Fixture::new();
    fx.write("output.tmpl", "x\n");
    let out = target(
        "escape",
        "../escape.md",
        serde_json::json!([{"kind": "template", "path": "output.tmpl"}]),
    );
    let err = template_doc::render_target(fx.repo_root(), &out, None, None, ExecIo::new())
        .expect_err("traversal out must fail");
    assert!(err.to_string().contains("traversal") || err.to_string().contains("escapes"));
}

// ---------------------------------------------------------------------------
// Plugin isolation: command-ref data arrives via the provider interface.
// ---------------------------------------------------------------------------

#[test]
fn command_ref_provider_lists_write() {
    let provider = CommandRefProvider;
    assert_eq!(provider.name(), "command-ref");
    let fragments = provider.fragments();
    let names: Vec<_> = fragments.iter().map(|f| f.name.as_str()).collect();
    assert!(names.contains(&"command_index"));
    assert!(names.contains(&"command_body"));
    let body = fragments
        .iter()
        .find(|f| f.name == "command_body")
        .expect("body");
    assert!(body.contents.contains("### WRITE"));
}

// ---------------------------------------------------------------------------
// Discovery: one system — `target.json` directories, member-attributed.
// ---------------------------------------------------------------------------

#[test]
fn member_for_dir_prefers_longest_prefix() {
    let members = vec![
        "crates/oxdock-logic-tests".to_string(),
        "crates/oxdock-logic-tests/fixtures/commands/ast_commands".to_string(),
    ];
    assert_eq!(
        member_for_dir(
            &members,
            "crates/oxdock-logic-tests/fixtures/commands/ast_commands/.oxdock/template"
        )
        .as_deref(),
        Some("crates/oxdock-logic-tests/fixtures/commands/ast_commands")
    );
    assert_eq!(
        member_for_dir(&members, "crates/oxdock-logic-tests/.oxdock/template").as_deref(),
        Some("crates/oxdock-logic-tests")
    );
    assert_eq!(member_for_dir(&members, ".oxdock/template/readme"), None);
    assert_eq!(member_for_dir(&members, "other/dir"), None);
}

#[test]
#[cfg_attr(
    miri,
    ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
)]
fn discover_finds_target_json_dirs_only() {
    let fx = Fixture::new();
    fx.write(
        "crates/alpha/.oxdock/template/target.json",
        r#"{"name":"alpha-readme","out":"crates/alpha/README.md",
            "stages":[{"kind":"text","text":"hi"}]}"#,
    );
    // No target.json: not a target (shared defaults, pilot-only, …).
    fx.write("crates/alpha/.oxdock/template/_global/values.json", "{}");
    fx.write(
        ".oxdock/template/readme/target.json",
        r#"{"name":"readme","out":"README.md",
        "stages":[{"kind":"text","text":"root"}]}"#,
    );
    let config = DocsGenConfig {
        template_roots: vec![".oxdock/template".to_string()],
        ..DocsGenConfig::default()
    };
    let found = discover_targets(
        fx.repo_root(),
        &config,
        &["crates/alpha".to_string(), "crates/alpha".to_string()],
    )
    .expect("discover");
    assert_eq!(found.len(), 2);
    let alpha = found
        .iter()
        .find(|t| t.spec.name == "alpha-readme")
        .expect("alpha");
    assert_eq!(alpha.spec.member.as_deref(), Some("crates/alpha"));
    let root = found
        .iter()
        .find(|t| t.spec.name == "readme")
        .expect("root");
    assert_eq!(root.spec.member, None);
}

#[test]
#[cfg_attr(
    miri,
    ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
)]
fn sparse_target_synthesizes_local_convention() {
    // A two-line target.json (nothing to get wrong) expands to the local
    // layout: header.md.tmpl + fragments globs + footer.md.tmpl, all owned
    // by the target directory. No document vocabulary in the engine.
    let fx = Fixture::new();
    fx.write(
        "crates/beta/.oxdock/template/target.json",
        r#"{"name":"beta","out":"crates/beta/README.md"}"#,
    );
    fx.write(
        "crates/beta/.oxdock/template/header.md.tmpl",
        "# {{ $docs_ctx.name }}\n",
    );
    fx.write("crates/beta/.oxdock/template/fragments/a.md", "A\n");
    fx.write("crates/beta/.oxdock/template/footer.md.tmpl", "F\n");
    let config = DocsGenConfig::default();
    let found =
        discover_targets(fx.repo_root(), &config, &["crates/beta".to_string()]).expect("discover");
    assert_eq!(found.len(), 1);
    let kinds: Vec<_> = found[0]
        .spec
        .stages
        .iter()
        .map(|s| s.kind.as_str())
        .collect();
    assert_eq!(kinds, ["template", "glob", "glob", "template"]);
    assert_eq!(
        found[0].spec.stages[0].path.as_deref(),
        Some("crates/beta/.oxdock/template/header.md.tmpl")
    );
    assert_eq!(found[0].spec.member.as_deref(), Some("crates/beta"));
}

#[test]
#[cfg_attr(
    miri,
    ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
)]
fn sparse_target_without_wrappers_renders_fragments_only() {
    // No header/footer files: convention yields just the fragment globs.
    // Other formats use explicit `stages` (see the pilots); the sparse
    // form covers the common collect-and-concatenate case.
    let fx = Fixture::new();
    fx.write(
        "crates/gamma/.oxdock/template/target.json",
        r#"{"name":"gamma","out":"crates/gamma/out.md"}"#,
    );
    fx.write("crates/gamma/.oxdock/template/fragments/a.md", "A\n");
    let config = DocsGenConfig::default();
    let found =
        discover_targets(fx.repo_root(), &config, &["crates/gamma".to_string()]).expect("discover");
    let kinds: Vec<_> = found[0]
        .spec
        .stages
        .iter()
        .map(|s| s.kind.as_str())
        .collect();
    assert_eq!(kinds, ["glob", "glob"]);
    template_doc::render_target(fx.repo_root(), &found[0].spec, None, None, ExecIo::new())
        .expect("render");
    assert_eq!(fx.read("crates/gamma/out.md"), "A\n");
}

// ---------------------------------------------------------------------------
// Config validation: unknown stage kinds fail fast.
// ---------------------------------------------------------------------------

#[test]
#[cfg_attr(
    miri,
    ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
)]
fn read_stage_orders_files_explicitly() {
    // `read` stages concatenate verbatim in declared order — no
    // filename-prefix ordering hacks, and generated files can live
    // outside the fragments dir for reuse across targets.
    let fx = Fixture::new();
    fx.write("sections/b.md", "B\n");
    fx.write("sections/a.md", "A\n");
    fx.write("generated/shared.md", "S\n");
    let out = target(
        "ordered",
        "out.md",
        serde_json::json!([
            {"kind": "read", "path": "sections/b.md"},
            {"kind": "read", "path": "generated/shared.md"},
            {"kind": "read", "path": "sections/a.md"},
        ]),
    );
    template_doc::render_target(fx.repo_root(), &out, None, None, ExecIo::new()).expect("render");
    assert_eq!(fx.read("out.md"), "B\nS\nA\n");
}

#[test]
#[cfg_attr(
    miri,
    ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
)]
fn template_target_derives_order_from_includes() {
    // No stage list managed: order comes from `{{> }}` positions in the
    // master document; literal prose expands with the values context.
    let fx = Fixture::new();
    fx.write("values.json", r#"{"who":"world"}"#);
    fx.write("body.md", "verbatim `{{ env:KEPT }}`\n");
    fx.write("sig.tmpl", "signed {{ $docs_ctx.who }}\n");
    fx.write(
        "output.tmpl",
        "Hello {{ $docs_ctx.who }}!\n{{> body.md }}\n{{> sig.tmpl }}\n",
    );
    let out = TargetSpec {
        name: "master".to_string(),
        out: "out.md".to_string(),
        values: Some("values.json".to_string()),
        stages: Vec::new(),
        member: None,
        template: Some("output.tmpl".to_string()),
    };
    template_doc::render_target(fx.repo_root(), &out, None, None, ExecIo::new()).expect("render");
    assert_eq!(
        fx.read("out.md"),
        "Hello world!\nverbatim `{{ env:KEPT }}`\nsigned world\n"
    );
}

#[test]
#[cfg_attr(
    miri,
    ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
)]
fn missing_include_fails_the_target() {
    // Strict guarantee: a master template referencing a path that does not
    // exist must fail rendering (never silently skip or emit partial
    // output), so a typo'd include breaks the build instead of shipping a
    // truncated document.
    let fx = Fixture::new();
    fx.write(
        "output.md.tmpl",
        "Hello\n{{> fragments/does-not-exist.md }}\n",
    );
    let out = TargetSpec {
        name: "broken".to_string(),
        out: "out.md".to_string(),
        values: None,
        stages: Vec::new(),
        member: None,
        template: Some("output.md.tmpl".to_string()),
    };
    // The driver writes incrementally, so a failed run can leave bytes
    // behind; the guarantee is the Err itself, which main.rs propagates
    // into a non-zero exit before anything is committed.
    let err = template_doc::render_target(fx.repo_root(), &out, None, None, ExecIo::new())
        .expect_err("missing include must fail");
    assert!(
        err.to_string().contains("does-not-exist"),
        "error must name the missing path, got: {err:#}"
    );
}

#[test]
fn config_rejects_unknown_stage_kind() {
    let err = DocsGenConfig::from_json(
        r#"{"targets": [{"name": "bad", "out": "x.md",
           "stages": [{"kind": "nope"}]}]}"#,
    )
    .expect_err("unknown stage kind must fail");
    assert!(err.to_string().contains("unknown stage kind"));
}
