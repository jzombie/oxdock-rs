//! Single-site command registry for all OxDock commands.
//!
//! `declare_commands!` is the sole source of truth. It generates:
//! - StepKind enum — all command + structural AST variants
//! - `pub fn lower_command(name, raw_args)` — name-dispatched lowering
//! - `pub fn all_metadata()` — collects `CommandMeta` from all declarations
//!   plus `all_structural_metadata()` (structural statements are documented
//!   through the same pipeline so reference docs cannot drift).
//!
//! To add a command: add one block inside `declare_commands!`.
//! To add a structural statement: extend the `structural [...]` list,
//! `all_structural_metadata()`, and the `structural_metadata_covers_all_structural_kinds`
//! tripwire below.

use std::fmt;

use crate::ast::{Arg, Expr, IoBinding, IoStream, Step, WorkspaceTarget};
use crate::command::{ArgSpec, CommandMeta, Example, FlagSpec, FlagValueType, IoDirection, Stream};
use anyhow::{Result, anyhow, bail};
use indoc::indoc;

// ── Helpers ────────────────────────────────────────────────────────────────

fn join_args(args: Vec<Arg>, cmd_name: &str) -> Result<Arg> {
    if args.is_empty() {
        bail!("{cmd_name} requires at least one argument");
    }
    if args.len() == 1 {
        return Ok(args.into_iter().next().unwrap());
    }
    Ok(Arg::String(
        args.iter()
            .map(|a| a.as_str())
            .collect::<Vec<_>>()
            .join(" "),
        false,
    ))
}

fn quote_arg(s: &str) -> String {
    let is_safe = s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
        && !s.starts_with(|c: char| c.is_ascii_digit() || c == '-' || c == '/' || c == '.')
        && crate::Command::parse(s).is_none();
    if is_safe && !s.is_empty() {
        s.to_string()
    } else {
        format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
    }
}

fn quote_msg(s: &str) -> String {
    let safe = s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
        && !s.starts_with(|c: char| c.is_ascii_digit())
        && crate::Command::parse(s).is_none();
    if safe && !s.is_empty() {
        s.to_string()
    } else {
        format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
    }
}

fn quote_run(s: &str) -> String {
    if s.is_empty() || s.chars().any(|c| c == ';' || c == '\n') || s.contains("//") {
        return format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""));
    }
    s.split(' ')
        .map(|w| {
            if w.starts_with(|c: char| c.is_ascii_digit())
                || w.starts_with(['/', '.', '-', ':', '='])
            {
                format!("\"{}\"", w.replace('\\', "\\\\").replace('"', "\\\""))
            } else {
                w.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn fmt_io(b: &IoBinding) -> String {
    let s = match b.stream {
        IoStream::Stdin => "stdin",
        IoStream::Stdout => "stdout",
        IoStream::Stderr => "stderr",
    };
    if let Some(p) = &b.pipe {
        format!("{}=pipe:{}", s, p)
    } else {
        s.to_string()
    }
}

/// Parse a TIMEOUT duration token (`500ms`, `10s`, `2m`, `1h`; a bare
/// number means seconds).
pub fn parse_duration(s: &str) -> Result<std::time::Duration> {
    let (digits, unit_ms): (&str, u64) = if let Some(v) = s.strip_suffix("ms") {
        (v, 1)
    } else if let Some(v) = s.strip_suffix('s') {
        (v, 1_000)
    } else if let Some(v) = s.strip_suffix('m') {
        (v, 60_000)
    } else if let Some(v) = s.strip_suffix('h') {
        (v, 3_600_000)
    } else {
        (s, 1_000)
    };
    let n: u64 = digits
        .parse()
        .map_err(|_| anyhow!("invalid TIMEOUT duration: {s}"))?;
    let millis = n
        .checked_mul(unit_ms)
        .ok_or_else(|| anyhow!("TIMEOUT duration out of range: {s}"))?;
    if millis == 0 {
        bail!("TIMEOUT duration must be positive, got: {s}");
    }
    Ok(std::time::Duration::from_millis(millis))
}

/// Canonical display for a duration: largest exact unit (`500ms`, `10s`,
/// `2m`, `1h`), falling back to milliseconds. Round-trips through
/// [`parse_duration`].
pub fn format_duration(d: &std::time::Duration) -> String {
    let millis = d.as_millis();
    if millis.is_multiple_of(3_600_000) {
        format!("{}h", millis / 3_600_000)
    } else if millis.is_multiple_of(60_000) {
        format!("{}m", millis / 60_000)
    } else if millis.is_multiple_of(1_000) {
        format!("{}s", millis / 1_000)
    } else {
        format!("{millis}ms")
    }
}

// ── declare_commands! ──────────────────────────────────────────────────────

// Keywords parsed by PEG rules rather than plain-command lowering (`WITH_IO`,
// `AWAIT`, ...). When a line starts with one of these but fails to parse as
// such, lowering falls through here — explain the expected syntax instead of
// only reporting an unknown command.
fn unknown_command_error(name: &str, raw_args: &[Arg]) -> anyhow::Error {
    let received = raw_args
        .iter()
        .map(Arg::as_str)
        .collect::<Vec<_>>()
        .join(" ");
    let hint = structural_hint(name, &received).or_else(|| case_hint(name));
    match hint {
        Some(hint) => anyhow!("unknown command: {name}\n{hint}"),
        None => anyhow!("unknown command: {name}"),
    }
}

fn structural_hint(name: &str, received: &str) -> Option<String> {
    let got = if received.is_empty() {
        "nothing".to_string()
    } else {
        format!("`{received}`")
    };
    match name {
        "WITH_IO" => Some(with_io_hint(&got, received)),
        "AWAIT" => Some(format!(
            "AWAIT waits for a background task variable, e.g. `LET $t = ASYNC ECHO hi` then `AWAIT $t`; got {got}."
        )),
        "CANCEL" => Some(format!(
            "CANCEL stops a background task variable, e.g. `CANCEL $t` (from `LET $t = ASYNC ...`); got {got}."
        )),
        "ASYNC" => Some(format!(
            "ASYNC runs a command in the background, e.g. `ASYNC RUN ...`, `ASYNC {{ ... }}`, or `LET $t = ASYNC ...`; got {got}."
        )),
        "FOR" => Some(format!(
            "FOR loops need `FOR $item IN <expr> {{ ... }}` (or `FOR $key, $value IN <expr> {{ ... }}`); got {got}."
        )),
        "IF" => Some(format!(
            "IF needs a condition and a block, e.g. `IF true {{ ECHO yes }}`; got {got}."
        )),
        "ELSE" => Some(format!(
            "ELSE must directly follow an `IF ... {{ ... }}` block, e.g. `IF true {{ ECHO yes }} ELSE {{ ECHO no }}`; got {got}."
        )),
        "LET" => Some(format!(
            "LET assigns a variable, e.g. `LET $name = <expr>` or `LET $t = ASYNC ...`; got {got}."
        )),
        "TIMEOUT" => Some(format!(
            "TIMEOUT needs a duration and a command or block, e.g. `TIMEOUT 30s RUN ...`; got {got}."
        )),
        "INHERIT_ENV" => Some(format!(
            "INHERIT_ENV takes a key list, e.g. `INHERIT_ENV [HOME PATH]`; got {got}."
        )),
        _ => None,
    }
}

/// Diagnose a `WITH_IO` line that failed to parse: most often a malformed
/// binding list (bindings are bare streams or `<stream>=pipe:<name>`).
fn with_io_hint(got: &str, received: &str) -> String {
    const SYNTAX: &str =
        "WITH_IO needs `WITH_IO [bindings] <command>` or `WITH_IO [bindings] { <commands> }`";
    const BINDINGS: &str = "bindings are `stdin`, `stdout`, `stderr`, or `<stream>=pipe:<name>` (e.g. `[stdout=pipe:log]`)";
    if let Some(after_open) = received.strip_prefix('[') {
        match after_open.split_once(']') {
            None => {
                return format!("{SYNTAX}: missing closing `]` in the binding list; got {got}.");
            }
            Some((bindings, _)) => {
                for part in bindings.split(',') {
                    let part = part.trim();
                    if part.is_empty() {
                        continue;
                    }
                    let (stream, binding) = match part.split_once('=') {
                        Some((stream, binding)) => (stream.trim(), Some(binding.trim())),
                        None => (part, None),
                    };
                    if !matches!(stream, "stdin" | "stdout" | "stderr") {
                        return format!(
                            "{SYNTAX}: invalid stream `{stream}`; expected `stdin`, `stdout`, or `stderr`; got {got}."
                        );
                    }
                    let valid = match binding {
                        None => true,
                        Some(value) => value
                            .strip_prefix("pipe:")
                            .map(|pipe| !pipe.trim().is_empty())
                            .unwrap_or(false),
                    };
                    if !valid {
                        return format!(
                            "{SYNTAX}: invalid binding `{part}`; {BINDINGS}; got {got}."
                        );
                    }
                }
            }
        }
    }
    format!("{SYNTAX}; got {got}. {BINDINGS}.")
}

/// `echo hi` is almost certainly `ECHO hi`: commands are uppercase.
fn case_hint(name: &str) -> Option<String> {
    let upper = name.to_ascii_uppercase();
    if upper != name
        && all_metadata()
            .iter()
            .any(|meta| meta.name == upper.as_str())
    {
        return Some(format!("did you mean `{upper}`? commands are uppercase."));
    }
    None
}

macro_rules! declare_commands {
    (
        structural [
            $( $sname:ident $( { $( $sfname:ident : $sftype:ty ),* $(,)? } )? ),* $(,)?
        ]

        $(
            $cmd_ident:ident => [
                name: $name:expr,
                variant: $vname:ident $( { $( $vfname:ident : $vftype:ty ),* $(,)? } )? $( ( $( $ttuple:ty ),* $(,)? ) )?,
                syntax: $syntax:expr,
                summary: $summary:expr,
                description: $desc:expr,
                args: $args:expr,
                flags: $flags:expr,
                default_output: $out:expr,
                examples: $examples:expr,
                lower: $lower:expr,
            ]
        ),* $(,)?
    ) => {
        #[derive(Debug, Clone, Eq, PartialEq)]
        pub enum StepKind {
            $( $vname $( { $( $vfname : $vftype ),* } )? $( ( $( $ttuple ),* ) )?, )*
            $( $sname $( { $( $sfname : $sftype ),* } )?, )*
        }

        pub fn lower_command(name: &str, raw_args: Vec<Arg>) -> Result<StepKind> {
            match name {
                $(
                    s if s == $name => {
                        let meta = CommandMeta {
                            name: $name, syntax: $syntax, summary: $summary,
                            description: $desc, args: $args, flags: $flags,
                            default_output: $out, examples: $examples,
                        };
                        let (flags, positional) = crate::strip_flags(raw_args, &meta)?;
                        let lower_fn: fn(Vec<(String, Arg)>, Vec<Arg>) -> Result<StepKind> = $lower;
                        lower_fn(flags, positional)
                    }
                )*
                _ => Err(unknown_command_error(name, &raw_args)),
            }
        }

        pub fn all_metadata() -> Vec<CommandMeta> {
            let mut out = vec![
                $( CommandMeta {
                    name: $name, syntax: $syntax, summary: $summary,
                    description: $desc, args: $args, flags: $flags,
                    default_output: $out, examples: $examples,
                }, )*
            ];
            // Structural statements are registered separately (see
            // all_structural_metadata) but documented through the same
            // pipeline so docs-gen never drifts from the parser.
            out.extend(all_structural_metadata());
            out
        }
    };
}

declare_commands! {
    structural [
        WithIo { bindings: Vec<IoBinding>, cmd: Box<StepKind> },
        WithIoBlock { bindings: Vec<IoBinding> },
        For { key_var: Option<String>, var: String, in_expr: Expr, body: Vec<Step> },
        If { cond: Box<Expr>, then_body: Vec<Step>, else_ifs: Vec<(Box<Expr>, Vec<Step>)>, else_body: Option<Vec<Step>> },
        Assign { var: String, expr: Expr },
        AsyncBlock { body: Vec<Step> },
        AssignAsync { var: String, body: Vec<Step> },
        Await { var: String },
        Cancel { var: String },
        Timeout { duration: std::time::Duration, body: Vec<Step> },
    ]

    Workdir => [
        name: "WORKDIR",
        variant: Workdir(Arg),
        syntax: "WORKDIR <path>",
        summary: "Change the working directory.",
        description: "Sets the current working directory.",
        args: &[ ArgSpec { name: "path", arg_type: "string", description: "Directory to change to", io: IoDirection::Write, index: 0, required: true, fallback_stream: None } ],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "change working directory", fence_meta: None, code: indoc! {r#"
            WORKDIR project/src
            WRITE generated.txt generated-under-workdir
            ASSERT_FILE generated.txt generated-under-workdir
        "#} } ],
        lower: |_flags, args| {
            let path = args.into_iter().next().ok_or_else(|| anyhow!("WORKDIR requires a path"))?;
            Ok(StepKind::Workdir(path))
        },
    ],

    Workspace => [
        name: "WORKSPACE",
        variant: Workspace(WorkspaceTarget),
        syntax: "WORKSPACE SNAPSHOT|LOCAL",
        summary: "Switch workspace roots.",
        description: "SNAPSHOT or LOCAL root.",
        args: &[ ArgSpec { name: "target", arg_type: "SNAPSHOT|LOCAL", description: "Target root", io: IoDirection::Write, index: 0, required: true, fallback_stream: None } ],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "switch roots", fence_meta: None, code: indoc! {r#"WORKSPACE LOCAL"#} } ],
        lower: |_flags, args| {
            let target = args.into_iter().next().ok_or_else(|| anyhow!("WORKSPACE requires a target"))?;
            match target.as_str() {
                "SNAPSHOT" | "snapshot" => Ok(StepKind::Workspace(WorkspaceTarget::Snapshot)),
                "LOCAL" | "local" => Ok(StepKind::Workspace(WorkspaceTarget::Local)),
                other => bail!("unknown workspace target: {other}"),
            }
        },
    ],

    Env => [
        name: "ENV",
        variant: Env { key: String, value: Arg },
        syntax: "ENV KEY=value",
        summary: "Set an environment variable.",
        description: "Inserts or updates an env var.",
        args: &[ ArgSpec { name: "assignment", arg_type: "KEY=value", description: "KEY=value pair", io: IoDirection::Write, index: 0, required: true, fallback_stream: None } ],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "set env", fence_meta: None, code: indoc! {r#"ENV APP_MODE=production"#} } ],
        lower: |_flags, args| {
            let arg = args.into_iter().next().ok_or_else(|| anyhow!("ENV requires KEY=value"))?;
            let (k, v) = arg.as_str().split_once('=').ok_or_else(|| anyhow!("ENV requires KEY=value format"))?;
            let val = v.strip_prefix('"').and_then(|s| s.strip_suffix('"')).unwrap_or(v);
            Ok(StepKind::Env { key: k.to_string(), value: Arg::String(val.to_string(), false) })
        },
    ],

    InheritEnv => [
        name: "INHERIT_ENV",
        variant: InheritEnv { keys: Vec<String> },
        syntax: "INHERIT_ENV <key>...",
        summary: "Inherit env vars from host.",
        description: "Declares which host environment variables to inherit into the script. Must appear before any other commands and at most once. Without this directive, the script starts with an empty environment.",
        args: &[],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "inherit env", fence_meta: None, code: indoc! {r#"INHERIT_ENV [PATH, HOME]"#} } ],
        lower: |_flags, args| {
            let keys = args.into_iter().map(|a| a.as_str().to_string()).collect();
            Ok(StepKind::InheritEnv { keys })
        },
    ],

    Echo => [
        name: "ECHO",
        variant: Echo(Arg),
        syntax: "ECHO <message>",
        summary: "Print to stdout.",
        description: "Outputs message to stdout.",
        args: &[ ArgSpec { name: "message", arg_type: "string", description: "Text", io: IoDirection::Write, index: 0, required: true, fallback_stream: None } ],
        flags: &[],
        default_output: Some(Stream::Stdout),
        examples: &[ Example { name: "echo", fence_meta: None, code: indoc! {r#"ECHO build-complete"#} } ],
        lower: |_flags, args| Ok(StepKind::Echo(join_args(args, "ECHO")?)),
    ],

    Run => [
        name: "RUN",
        variant: Run(Arg),
        syntax: "RUN <command...>",
        summary: "Execute shell command.",
        description: "Runs command in cwd.",
        args: &[ ArgSpec { name: "command", arg_type: "string...", description: "Command", io: IoDirection::Write, index: 0, required: true, fallback_stream: None } ],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "run", fence_meta: None, code: indoc! {r#"RUN echo hello"#} } ],
        lower: |_flags, args| Ok(StepKind::Run(join_args(args, "RUN")?)),
    ],

    Copy => [
        name: "COPY",
        variant: Copy { from_current_workspace: bool, from: Arg, to: Arg },
        syntax: "COPY [--from-current-workspace] <from> <to>",
        summary: "Copy file into workspace.",
        description: "Copies from host.",
        args: &[
            ArgSpec { name: "from", arg_type: "path", description: "Source", io: IoDirection::Read, index: 0, required: true, fallback_stream: None },
            ArgSpec { name: "to", arg_type: "path", description: "Dest", io: IoDirection::Write, index: 1, required: true, fallback_stream: None },
        ],
        flags: &[ FlagSpec { name: "from_current_workspace", long: "--from-current-workspace", value_type: FlagValueType::Flag, required: false, description: "From workspace root" } ],
        default_output: None,
        examples: &[ Example { name: "copy", fence_meta: Some("roots:unified"), code: indoc! {r#"
            WRITE src.txt content
            COPY src.txt dst.txt
            ASSERT_FILE dst.txt content
        "#} } ],
        lower: |flags, args| {
            let from_current_workspace = flags.iter().any(|(k, _)| k == "from_current_workspace");
            let mut it = args.into_iter();
            let from = it.next().ok_or_else(|| anyhow!("COPY requires a source"))?;
            let to = it.next().ok_or_else(|| anyhow!("COPY requires a destination"))?;
            Ok(StepKind::Copy { from_current_workspace, from, to })
        },
    ],

    CopyGit => [
        name: "COPY_GIT",
        variant: CopyGit { rev: Arg, from: Arg, to: Arg, include_dirty: bool },
        syntax: "COPY_GIT [--include-dirty] <rev> <src> <dst>",
        summary: "Copy from git revision.",
        description: "Checkout and copy.",
        args: &[
            ArgSpec { name: "rev", arg_type: "string", description: "Rev", io: IoDirection::Read, index: 0, required: true, fallback_stream: None },
            ArgSpec { name: "src", arg_type: "path", description: "Src", io: IoDirection::Read, index: 1, required: true, fallback_stream: None },
            ArgSpec { name: "dst", arg_type: "path", description: "Dst", io: IoDirection::Write, index: 2, required: true, fallback_stream: None },
        ],
        flags: &[ FlagSpec { name: "dirty", long: "--include-dirty", value_type: FlagValueType::Flag, required: false, description: "Include dirty" } ],
        default_output: None,
        examples: &[ Example { name: "git copy", fence_meta: Some("expect_error:\"COPY source missing\""), code: indoc! {r#"COPY_GIT HEAD src.txt dst.txt"#} } ],
        lower: |flags, args| {
            let include_dirty = flags.iter().any(|(k, _)| k == "dirty");
            let mut it = args.into_iter();
            let rev = it.next().ok_or_else(|| anyhow!("COPY_GIT requires a revision"))?;
            let from = it.next().ok_or_else(|| anyhow!("COPY_GIT requires a source"))?;
            let to = it.next().ok_or_else(|| anyhow!("COPY_GIT requires a destination"))?;
            Ok(StepKind::CopyGit { rev, from, to, include_dirty })
        },
    ],

    Symlink => [
        name: "SYMLINK",
        variant: Symlink { from: Arg, to: Arg },
        syntax: "SYMLINK <from> <to>",
        summary: "Create symlink.",
        description: "Creates symlink.",
        args: &[
            ArgSpec { name: "from", arg_type: "path", description: "Target", io: IoDirection::Read, index: 0, required: true, fallback_stream: None },
            ArgSpec { name: "to", arg_type: "path", description: "Link", io: IoDirection::Write, index: 1, required: true, fallback_stream: None },
        ],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "symlink", fence_meta: Some("roots:unified"), code: indoc! {r#"
            WRITE original.txt content
            SYMLINK original.txt link.txt
            ASSERT_FILE link.txt content
        "#} } ],
        lower: |_flags, args| {
            let mut it = args.into_iter();
            let from = it.next().ok_or_else(|| anyhow!("SYMLINK requires a source"))?;
            let to = it.next().ok_or_else(|| anyhow!("SYMLINK requires a target"))?;
            Ok(StepKind::Symlink { from, to })
        },
    ],

    Mkdir => [
        name: "MKDIR",
        variant: Mkdir(Arg),
        syntax: "MKDIR <path>",
        summary: "Create directory.",
        description: "Creates dir with parents.",
        args: &[ ArgSpec { name: "path", arg_type: "path", description: "Dir path", io: IoDirection::Write, index: 0, required: true, fallback_stream: None } ],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "mkdir", fence_meta: None, code: indoc! {r#"MKDIR deeply/nested/tree"#} } ],
        lower: |_flags, args| Ok(StepKind::Mkdir(args.into_iter().next().ok_or_else(|| anyhow!("MKDIR requires a path"))?)),
    ],

    Ls => [
        name: "LS",
        variant: Ls(Option<Arg>),
        syntax: "LS [<path>]",
        summary: "List directory.",
        description: "Lists entries.",
        args: &[ ArgSpec { name: "path", arg_type: "path", description: "Dir", io: IoDirection::Read, index: 0, required: false, fallback_stream: None } ],
        flags: &[],
        default_output: Some(Stream::Stdout),
        examples: &[ Example { name: "ls", fence_meta: None, code: indoc! {r#"
            MKDIR inventory
            WRITE inventory/a.txt a
            LS inventory
        "#} } ],
        lower: |_flags, args| Ok(StepKind::Ls(args.into_iter().next())),
    ],

    Cwd => [
        name: "CWD",
        variant: Cwd,
        syntax: "CWD",
        summary: "Print working directory.",
        description: "Outputs cwd.",
        args: &[],
        flags: &[],
        default_output: Some(Stream::Stdout),
        examples: &[ Example { name: "cwd", fence_meta: None, code: indoc! {r#"CWD"#} } ],
        lower: |_flags, _args| Ok(StepKind::Cwd),
    ],

    Read => [
        name: "READ",
        variant: Read(Option<Arg>),
        syntax: "READ [<path>]",
        summary: "Read file to stdout.",
        description: "Outputs file contents.",
        args: &[ ArgSpec { name: "path", arg_type: "path", description: "File", io: IoDirection::Read, index: 0, required: false, fallback_stream: None } ],
        flags: &[],
        default_output: Some(Stream::Stdout),
        examples: &[ Example { name: "read", fence_meta: None, code: indoc! {r#"
            WRITE note.txt "hello"
            READ note.txt
        "#} } ],
        lower: |_flags, args| Ok(StepKind::Read(args.into_iter().next())),
    ],

    ReadLine => [
        name: "READ_LINE",
        variant: ReadLine { var: String },
        syntax: "READ_LINE $var",
        summary: "Read one line from stdin into a variable.",
        description: "Reads bytes until newline without waiting for EOF, leaving the pipe open. Trailing newline is stripped (shell-read parity). On premature EOF assigns accumulated bytes and returns.",
        args: &[ ArgSpec { name: "var", arg_type: "$var", description: "Variable to store the line", io: IoDirection::Write, index: 0, required: true, fallback_stream: None } ],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "read line", fence_meta: None, code: indoc! {r#"
            WITH_IO [stdout=pipe:lines] ECHO "first"
            WITH_IO [stdin=pipe:lines] READ_LINE $reply
        "#} } ],
        lower: |_flags, args| {
            let arg = args.into_iter().next().ok_or_else(|| anyhow!("READ_LINE requires a variable"))?;
            let var = match arg {
                Arg::Expr(Expr::Var(name)) => name,
                Arg::String(s, _) => s.trim_start_matches('$').to_string(),
                other => bail!("READ_LINE requires a $variable, found {:?}", other),
            };
            if var.is_empty() {
                bail!("READ_LINE requires a variable");
            }
            Ok(StepKind::ReadLine { var })
        },
    ],

    Write => [
        name: "WRITE",
        variant: Write { path: Arg, contents: Option<Arg> },
        syntax: "WRITE <path> [<contents>]",
        summary: "Write to file.",
        description: "Writes contents.",
        args: &[
            ArgSpec { name: "path", arg_type: "path", description: "File", io: IoDirection::Write, index: 0, required: true, fallback_stream: None },
            ArgSpec { name: "contents", arg_type: "string", description: "Content", io: IoDirection::Write, index: 1, required: false, fallback_stream: Some(Stream::Stdin) },
        ],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "write", fence_meta: None, code: indoc! {r#"WRITE output.txt hello-world"#} } ],
        lower: |_flags, args| {
            let mut it = args.into_iter();
            let path = it.next().ok_or_else(|| anyhow!("WRITE requires a path"))?;
            let remaining: Vec<Arg> = it.collect();
            let contents = if remaining.is_empty() { None } else { Some(join_args(remaining, "WRITE")?) };
            Ok(StepKind::Write { path, contents })
        },
    ],

    Append => [
        name: "APPEND",
        variant: Append { path: Arg, contents: Option<Arg> },
        syntax: "APPEND <path> [<contents>]",
        summary: "Append to file.",
        description: "Appends contents.",
        args: &[
            ArgSpec { name: "path", arg_type: "path", description: "File", io: IoDirection::Write, index: 0, required: true, fallback_stream: None },
            ArgSpec { name: "contents", arg_type: "string", description: "Content", io: IoDirection::Write, index: 1, required: false, fallback_stream: Some(Stream::Stdin) },
        ],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "append", fence_meta: None, code: indoc! {r#"
            WRITE log.txt line1
            APPEND log.txt line2
            ASSERT_FILE log.txt line1line2
        "#} } ],
        lower: |_flags, args| {
            let mut it = args.into_iter();
            let path = it.next().ok_or_else(|| anyhow!("APPEND requires a path"))?;
            let remaining: Vec<Arg> = it.collect();
            let contents = if remaining.is_empty() { None } else { Some(join_args(remaining, "APPEND")?) };
            Ok(StepKind::Append { path, contents })
        },
    ],

    Expand => [
        name: "EXPAND",
        variant: Expand { path: Option<Arg>, overrides: Vec<(String, Arg)> },
        syntax: "EXPAND [<path>] [<KEY=val> ...]",
        summary: "Expand templates.",
        description: "Expands placeholders.",
        args: &[ ArgSpec { name: "path", arg_type: "path", description: "Template", io: IoDirection::Read, index: 0, required: false, fallback_stream: None } ],
        flags: &[],
        default_output: Some(Stream::Stdout),
        examples: &[ Example { name: "expand", fence_meta: None, code: indoc! {r#"
            ENV NAME="Alice"
            WRITE template.md "Hello {{ env:NAME }}!"
            EXPAND template.md
            ASSERT_STDOUT "Hello Alice!"
        "#} } ],
        lower: |_flags, args| {
            let mut path = None;
            let mut overrides = Vec::new();
            for arg in args {
                let s = arg.as_str();
                if let Some((k, v)) = s.split_once('=') {
                    let val = v.strip_prefix('"').and_then(|s| s.strip_suffix('"'))
                        .or_else(|| v.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')))
                        .unwrap_or(v);
                    overrides.push((k.to_string(), Arg::String(val.to_string(), false)));
                } else if path.is_none() { path = Some(arg); }
                else { bail!("EXPAND accepts at most one path"); }
            }
            Ok(StepKind::Expand { path, overrides })
        },
    ],

    AssertFile => [
        name: "ASSERT_FILE",
        variant: AssertFile { hash: Option<String>, path: Arg, contents: Option<Arg> },
        syntax: "ASSERT_FILE [--hash <sha256>] <path> [<expected>]",
        summary: "Assert file exists.",
        description: "Verifies file.",
        args: &[
            ArgSpec { name: "path", arg_type: "path", description: "File", io: IoDirection::Read, index: 0, required: true, fallback_stream: None },
            ArgSpec { name: "expected", arg_type: "string", description: "Expected", io: IoDirection::Read, index: 1, required: false, fallback_stream: None },
        ],
        flags: &[ FlagSpec { name: "hash", long: "--hash", value_type: FlagValueType::String, required: false, description: "SHA-256" } ],
        default_output: None,
        examples: &[ Example { name: "assert file", fence_meta: None, code: indoc! {r#"
            WRITE payload.bin stable-content
            ASSERT_FILE payload.bin stable-content
        "#} } ],
        lower: |flags, args| {
            let hash = flags.iter().find(|(k, _)| k == "hash").map(|(_, v)| v.as_str().to_string());
            let mut it = args.into_iter();
            let path = it.next().ok_or_else(|| anyhow!("ASSERT_FILE requires a path"))?;
            let remaining: Vec<Arg> = it.collect();
            let contents = if remaining.is_empty() { None } else { Some(join_args(remaining, "ASSERT_FILE")?) };
            Ok(StepKind::AssertFile { hash, path, contents })
        },
    ],

    AssertDir => [
        name: "ASSERT_DIR",
        variant: AssertDir(Arg),
        syntax: "ASSERT_DIR <path>",
        summary: "Assert dir exists.",
        description: "Verifies dir.",
        args: &[ ArgSpec { name: "path", arg_type: "path", description: "Dir", io: IoDirection::Read, index: 0, required: true, fallback_stream: None } ],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "assert dir", fence_meta: None, code: indoc! {r#"
            MKDIR dist/assets
            ASSERT_DIR dist/assets
        "#} } ],
        lower: |_flags, args| Ok(StepKind::AssertDir(args.into_iter().next().ok_or_else(|| anyhow!("ASSERT_DIR requires a path"))?)),
    ],

    AssertAbsent => [
        name: "ASSERT_ABSENT",
        variant: AssertAbsent(Arg),
        syntax: "ASSERT_ABSENT <path>",
        summary: "Assert path absent.",
        description: "Verifies absence.",
        args: &[ ArgSpec { name: "path", arg_type: "path", description: "Path", io: IoDirection::Read, index: 0, required: true, fallback_stream: None } ],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "assert absent", fence_meta: None, code: indoc! {r#"ASSERT_ABSENT missing.txt"#} } ],
        lower: |_flags, args| Ok(StepKind::AssertAbsent(args.into_iter().next().ok_or_else(|| anyhow!("ASSERT_ABSENT requires a path"))?)),
    ],

    AssertStdout => [
        name: "ASSERT_STDOUT",
        variant: AssertStdout(Arg),
        syntax: "ASSERT_STDOUT <substring>",
        summary: "Assert stdout contains.",
        description: "Verifies stdout.",
        args: &[ ArgSpec { name: "substring", arg_type: "string", description: "Substring", io: IoDirection::Read, index: 0, required: true, fallback_stream: None } ],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "assert stdout", fence_meta: None, code: indoc! {r#"
            ECHO build-complete
            ASSERT_STDOUT build-complete
        "#} } ],
        lower: |_flags, args| Ok(StepKind::AssertStdout(join_args(args, "ASSERT_STDOUT")?)),
    ],

    HashSha256 => [
        name: "HASH_SHA256",
        variant: HashSha256 { path: Arg },
        syntax: "HASH_SHA256 <path>",
        summary: "Print SHA-256.",
        description: "Computes digest.",
        args: &[ ArgSpec { name: "path", arg_type: "path", description: "File", io: IoDirection::Read, index: 0, required: true, fallback_stream: None } ],
        flags: &[],
        default_output: Some(Stream::Stdout),
        examples: &[ Example { name: "hash", fence_meta: None, code: indoc! {r#"
            WRITE payload.txt hello
            HASH_SHA256 payload.txt
        "#} } ],
        lower: |_flags, args| Ok(StepKind::HashSha256 { path: args.into_iter().next().ok_or_else(|| anyhow!("HASH_SHA256 requires a path"))? }),
    ],

    Exit => [
        name: "EXIT",
        variant: Exit(i32),
        syntax: "EXIT <code>",
        summary: "Exit pipeline.",
        description: "Stops the pipeline immediately with an `EXIT requested with code <code>` error; steps after it never run, at any nesting depth. Enclosing blocks still unwind their LET/ENV/WORKDIR/WORKSPACE state, anonymous background tasks are killed synchronously, and files written before the EXIT persist.",
        args: &[ ArgSpec { name: "code", arg_type: "int", description: "Code", io: IoDirection::Write, index: 0, required: true, fallback_stream: None } ],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "exit", fence_meta: Some("expect_error:\"EXIT requested with code 0\""), code: indoc! {r#"EXIT 0"#} } ],
        lower: |_flags, args| {
            let code = args.into_iter().next().and_then(|a| a.as_str().parse::<i32>().ok()).unwrap_or(0);
            Ok(StepKind::Exit(code))
        },
    ],

    Sleep => [
        name: "SLEEP",
        variant: Sleep { duration: std::time::Duration },
        syntax: "SLEEP <duration>",
        summary: "Sleep without spawning a shell.",
        description: "Parks the step for the duration (e.g. 500ms, 10s, 2m). Cooperative: checks for cancellation so an enclosing TIMEOUT or task teardown interrupts the sleep. Cross-platform alternative to shell sleep for testing time boundaries.",
        args: &[ ArgSpec { name: "duration", arg_type: "duration", description: "How long to sleep", io: IoDirection::Write, index: 0, required: true, fallback_stream: None } ],
        flags: &[],
        default_output: None,
        examples: &[ Example { name: "sleep", fence_meta: None, code: indoc! {r#"SLEEP 100ms"#} } ],
        lower: |_flags, args| {
            let mut it = args.into_iter();
            let raw = it
                .next()
                .ok_or_else(|| anyhow!("SLEEP requires a duration (e.g. SLEEP 500ms)"))?;
            if it.next().is_some() {
                bail!("SLEEP takes exactly one duration argument");
            }
            Ok(StepKind::Sleep {
                duration: parse_duration(raw.as_str())?,
            })
        },
    ],
}

// ── Structural metadata ──────────────────────────────────────────────────
// Single source of truth for structural-statement documentation (TIMEOUT,
// ASYNC, AWAIT, WITH_IO, IF, FOR, ...). These constructs are parsed by PEG
// rules rather than `declare_commands!`, so their reference docs live here
// instead of `crates/docs-gen/src/command_ref.rs` — adding a structural
// StepKind without registering it here fails `structural_metadata_covers_all_structural_kinds`
// below, and docs-gen renders these entries dynamically (no hardcoded copy).
pub fn all_structural_metadata() -> Vec<CommandMeta> {
    vec![
        CommandMeta {
            name: "WITH_IO",
            syntax: "WITH_IO [bindings] <command> | WITH_IO [bindings] { <commands> }",
            summary: "Reroute standard streams.",
            description: "Reroutes the standard streams of the next command or, in block form, of every enclosed command. Bindings map streams (`stdin`, `stdout`, `stderr`) to named pipes (`stdout=pipe:name`). Pipe names registered by the host runtime tee structured output elsewhere; a name bound as output can later feed another command's `stdin`, connecting commands without touching the terminal. Nested blocks stack defaults; inline bindings override inherited ones for their command only; closing a block restores previous wiring.",
            args: &[],
            flags: &[],
            default_output: None,
            examples: &[Example {
                name: "with_io block",
                fence_meta: None,
                code: indoc! {r#"
                WITH_IO [stdout=pipe:log] {
                  ECHO first
                  ECHO second
                }
                WITH_IO [stdin=pipe:log] WRITE captured.txt
            "#},
            }],
        },
        CommandMeta {
            name: "FOR",
            syntax: "FOR $item IN <expr> { <commands> } | FOR $key, $value IN <expr> { <commands> }",
            summary: "Iterate over a list or map.",
            description: "The loop variable receives each element (lists) or value (maps); with two variables, the first receives the key.",
            args: &[],
            flags: &[],
            default_output: None,
            examples: &[Example {
                name: "for loop",
                fence_meta: None,
                code: indoc! {r#"
                LET $items = ["a", "b"]
                FOR $item IN $items {
                  ECHO $item
                }

                LET $map = {"x": 1}
                FOR $k, $v IN $map {
                  ECHO "$k=$v"
                }
            "#},
            }],
        },
        CommandMeta {
            name: "IF",
            syntax: "IF <expr> { <commands> } [ELSE IF <expr> { <commands> }] [ELSE { <commands> }]",
            summary: "Conditional execution.",
            description: "The condition is evaluated as a boolean expression. Prefix `!` negates (`IF !false`); only Bool values are accepted as conditions.",
            args: &[],
            flags: &[],
            default_output: None,
            examples: &[Example {
                name: "if else",
                fence_meta: None,
                code: indoc! {r#"
                IF true {
                  ECHO yes
                } ELSE {
                  ECHO no
                }

                IF false {
                  ECHO skipped
                } ELSE IF true {
                  ECHO fallback
                }

                IF !false {
                  ECHO inverted
                }
            "#},
            }],
        },
        CommandMeta {
            name: "LET",
            syntax: "LET $var = <expr> | LET $var = ASYNC { <commands> }",
            summary: "Bind script-local variables.",
            description: "Assigns a value to a script-local variable. Variables are usable in templates (`{{ $var }}`), guards, and expressions. With `ASYNC`, spawns a background task and stores its handle (see ASYNC).",
            args: &[],
            flags: &[],
            default_output: None,
            examples: &[Example {
                name: "let",
                fence_meta: None,
                code: indoc! {r#"
                LET $name = "world"
                ECHO "hello, {{ $name }}"

                LET $items = ["a", "b"]
                LET $count = 42
            "#},
            }],
        },
        CommandMeta {
            name: "ASYNC",
            syntax: "ASYNC <command...> | ASYNC { <commands> } | LET $var = ASYNC { <commands> }",
            summary: "Run steps in a background thread.",
            description: "Runs a command or block of commands in a background thread with subshell isolation. Mutations (ENV, WORKDIR) stay within the block. With `LET`, stores a task handle for `AWAIT`.",
            args: &[],
            flags: &[],
            default_output: None,
            examples: &[
                Example {
                    name: "async",
                    fence_meta: None,
                    code: indoc! {r#"
                    ASYNC ECHO "first"

                    ASYNC {
                        ECHO "first"
                        ECHO "second"
                    }
                "#},
                },
                Example {
                    name: "async task handle",
                    fence_meta: None,
                    code: indoc! {r#"
                    LET $task = ASYNC {
                        ECHO "built"
                    }
                    AWAIT $task
                "#},
                },
            ],
        },
        CommandMeta {
            name: "AWAIT",
            syntax: "AWAIT $var",
            summary: "Join a background task.",
            description: "Blocks until the named task completes. Propagates errors if the task failed.",
            args: &[],
            flags: &[],
            default_output: None,
            examples: &[Example {
                name: "await",
                fence_meta: None,
                code: indoc! {r#"
                LET $task = ASYNC ECHO "done"
                AWAIT $task
            "#},
            }],
        },
        CommandMeta {
            name: "CANCEL",
            syntax: "CANCEL $var",
            summary: "Synchronously cancel a background task.",
            description: "Kills the named background task spawned via LET $var = ASYNC .... Blocking: returns only after the task thread has been joined and its OS process reaped, so no residual filesystem or stream mutation follows. A later AWAIT $var reports cancellation. Only named tasks can be cancelled.",
            args: &[],
            flags: &[],
            default_output: None,
            examples: &[Example {
                name: "cancel",
                fence_meta: None,
                code: indoc! {r#"
                LET $task = ASYNC SLEEP 30s
                CANCEL $task
            "#},
            }],
        },
        CommandMeta {
            name: "TIMEOUT",
            syntax: "TIMEOUT <duration> <command...> | TIMEOUT <duration> { <commands> } | TIMEOUT <duration> AWAIT $var",
            summary: "Enforce an execution deadline.",
            description: "Aborts the wrapped step or block with a deadline error if it exceeds the duration (e.g. 500ms, 10s, 2m; a bare number means seconds). A blocking foreground process is killed.",
            args: &[],
            flags: &[],
            default_output: None,
            examples: &[
                Example {
                    name: "timeout",
                    fence_meta: None,
                    code: indoc! {r#"TIMEOUT 30s WRITE heartbeat.txt alive"#},
                },
                Example {
                    name: "timeout block",
                    fence_meta: None,
                    code: indoc! {r#"
                    TIMEOUT 30s {
                        WRITE a.txt one
                        WRITE b.txt two
                    }
                "#},
                },
            ],
        },
    ]
}

// ── Display ────────────────────────────────────────────────────────────────

impl fmt::Display for StepKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StepKind::InheritEnv { keys } => write!(f, "INHERIT_ENV [{}]", keys.join(", ")),
            StepKind::Workdir(a) => write!(f, "WORKDIR {}", quote_arg(a.as_str())),
            StepKind::Workspace(t) => write!(f, "WORKSPACE {}", t),
            StepKind::Env { key, value } => write!(f, "ENV {}={}", key, quote_arg(value.as_str())),
            StepKind::Run(c) => write!(f, "RUN {}", quote_run(c.as_str())),
            StepKind::Echo(m) => write!(f, "ECHO {}", quote_msg(m.as_str())),
            StepKind::Copy {
                from_current_workspace,
                from,
                to,
            } => {
                if *from_current_workspace {
                    write!(
                        f,
                        "COPY --from-current-workspace {} {}",
                        quote_arg(from.as_str()),
                        quote_arg(to.as_str())
                    )
                } else {
                    write!(
                        f,
                        "COPY {} {}",
                        quote_arg(from.as_str()),
                        quote_arg(to.as_str())
                    )
                }
            }
            StepKind::Symlink { from, to } => write!(
                f,
                "SYMLINK {} {}",
                quote_arg(from.as_str()),
                quote_arg(to.as_str())
            ),
            StepKind::Mkdir(a) => write!(f, "MKDIR {}", quote_arg(a.as_str())),
            StepKind::Ls(a) => {
                write!(f, "LS")?;
                if let Some(x) = a {
                    write!(f, " {}", quote_arg(x.as_str()))?;
                }
                Ok(())
            }
            StepKind::Cwd => write!(f, "CWD"),
            StepKind::Read(a) => {
                write!(f, "READ")?;
                if let Some(x) = a {
                    write!(f, " {}", quote_arg(x.as_str()))?;
                }
                Ok(())
            }
            StepKind::ReadLine { var } => write!(f, "READ_LINE ${}", var),
            StepKind::Write { path, contents } => {
                write!(f, "WRITE {}", quote_arg(path.as_str()))?;
                if let Some(b) = contents {
                    write!(f, " {}", quote_msg(b.as_str()))?;
                }
                Ok(())
            }
            StepKind::Append { path, contents } => {
                write!(f, "APPEND {}", quote_arg(path.as_str()))?;
                if let Some(b) = contents {
                    write!(f, " {}", quote_msg(b.as_str()))?;
                }
                Ok(())
            }
            StepKind::Expand { path, overrides } => {
                write!(f, "EXPAND")?;
                if let Some(p) = path {
                    write!(f, " {}", quote_arg(p.as_str()))?;
                }
                for (k, v) in overrides {
                    write!(f, " {}={}", k, quote_arg(v.as_str()))?;
                }
                Ok(())
            }
            StepKind::AssertFile {
                hash,
                path,
                contents,
            } => {
                if let Some(d) = hash {
                    write!(f, "ASSERT_FILE --hash {} {}", d, quote_arg(path.as_str()))
                } else {
                    write!(f, "ASSERT_FILE {}", quote_arg(path.as_str()))?;
                    if let Some(b) = contents {
                        write!(f, " {}", quote_msg(b.as_str()))?;
                    }
                    Ok(())
                }
            }
            StepKind::AssertDir(a) => write!(f, "ASSERT_DIR {}", quote_arg(a.as_str())),
            StepKind::AssertAbsent(a) => write!(f, "ASSERT_ABSENT {}", quote_arg(a.as_str())),
            StepKind::AssertStdout(m) => write!(f, "ASSERT_STDOUT {}", quote_msg(m.as_str())),
            StepKind::WithIo { bindings, cmd } => {
                let p: Vec<String> = bindings.iter().map(fmt_io).collect();
                write!(f, "WITH_IO [{}] {}", p.join(", "), cmd)
            }
            StepKind::WithIoBlock { bindings } => {
                let p: Vec<String> = bindings.iter().map(fmt_io).collect();
                write!(f, "WITH_IO [{}] {{...}}", p.join(", "))
            }
            StepKind::CopyGit {
                rev,
                from,
                to,
                include_dirty,
            } => {
                if *include_dirty {
                    write!(
                        f,
                        "COPY_GIT --include-dirty {} {} {}",
                        quote_arg(rev.as_str()),
                        quote_arg(from.as_str()),
                        quote_arg(to.as_str())
                    )
                } else {
                    write!(
                        f,
                        "COPY_GIT {} {} {}",
                        quote_arg(rev.as_str()),
                        quote_arg(from.as_str()),
                        quote_arg(to.as_str())
                    )
                }
            }
            StepKind::HashSha256 { path } => write!(f, "HASH_SHA256 {}", quote_arg(path.as_str())),
            StepKind::Exit(c) => write!(f, "EXIT {}", c),
            StepKind::Sleep { duration } => write!(f, "SLEEP {}", format_duration(duration)),
            StepKind::For {
                key_var,
                var,
                in_expr,
                body,
            } => {
                match key_var {
                    Some(k) => write!(f, "FOR ${}, ${} IN {} {{", k, var, in_expr)?,
                    None => write!(f, "FOR ${} IN {} {{", var, in_expr)?,
                }
                for s in body {
                    write!(f, "\n    {}", s)?;
                }
                write!(f, "\n}}")
            }
            StepKind::If {
                cond,
                then_body,
                else_ifs,
                else_body,
            } => {
                write!(f, "IF {} {{", cond)?;
                for s in then_body {
                    write!(f, "\n    {}", s)?;
                }
                write!(f, " }}")?;
                for (c, b) in else_ifs {
                    write!(f, " ELSE IF {} {{", c)?;
                    for s in b {
                        write!(f, "\n    {}", s)?;
                    }
                    write!(f, " }}")?;
                }
                if let Some(b) = else_body {
                    write!(f, " ELSE {{")?;
                    for s in b {
                        write!(f, "\n    {}", s)?;
                    }
                    write!(f, " }}")?;
                }
                Ok(())
            }
            StepKind::Assign { var, expr } => write!(f, "LET ${} = {}", var, expr),
            StepKind::AsyncBlock { body } => {
                write!(f, "ASYNC {{")?;
                for s in body {
                    write!(f, "\n    {}", s)?;
                }
                write!(f, "\n}}")
            }
            StepKind::AssignAsync { var, body } => {
                write!(f, "LET ${} = ASYNC {{", var)?;
                for s in body {
                    write!(f, "\n    {}", s)?;
                }
                write!(f, "\n}}")
            }
            StepKind::Await { var } => write!(f, "AWAIT ${}", var),
            StepKind::Cancel { var } => write!(f, "CANCEL ${}", var),
            StepKind::Timeout { duration, body } => {
                let budget = format_duration(duration);
                if body.len() == 1 {
                    write!(f, "TIMEOUT {} {}", budget, body[0].kind)
                } else {
                    write!(f, "TIMEOUT {} {{", budget)?;
                    for s in body {
                        write!(f, "\n    {}", s)?;
                    }
                    write!(f, "\n}}")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_script;

    fn parse_err(script: &str) -> String {
        parse_script(script, lower_command)
            .expect_err("script must fail to parse")
            .to_string()
    }

    #[test]
    fn malformed_with_io_binding_names_the_bad_binding() {
        let err = parse_err("WITH_IO [stdout=discard] ECHO \"test\"\n");
        assert!(err.contains("unknown command: WITH_IO"), "{err}");
        assert!(err.contains("stdout=discard"), "{err}");
        assert!(err.contains("pipe:<name>"), "{err}");
    }

    #[test]
    fn await_without_task_variable_points_at_syntax() {
        let err = parse_err("AWAIT ECHO \"test\"\n");
        assert!(err.contains("unknown command: AWAIT"), "{err}");
        assert!(err.contains("AWAIT $t"), "{err}");
        assert!(err.contains("ECHO"), "{err}");
    }

    #[test]
    fn genuinely_unknown_command_keeps_bare_message() {
        let err = parse_err("FROBNICATE hi\n");
        assert!(err.contains("unknown command: FROBNICATE"), "{err}");
        assert!(!err.contains("did you mean"), "{err}");
    }

    #[test]
    fn lowercase_command_suggests_uppercase() {
        // Lowercase never reaches lowering through `parse_script` (the
        // grammar rejects it with its own uppercase hint), so exercise the
        // public `lower_command` dispatcher directly.
        let err = lower_command("echo", vec![Arg::String("hi".to_string(), false)])
            .expect_err("must fail")
            .to_string();
        assert!(err.contains("unknown command: echo"), "{err}");
        assert!(err.contains("did you mean `ECHO`"), "{err}");
    }

    #[test]
    fn parse_duration_units() {
        use std::time::Duration;
        assert_eq!(parse_duration("500ms").unwrap(), Duration::from_millis(500));
        assert_eq!(parse_duration("10s").unwrap(), Duration::from_secs(10));
        assert_eq!(parse_duration("2m").unwrap(), Duration::from_secs(120));
        assert_eq!(parse_duration("1h").unwrap(), Duration::from_secs(3600));
        assert_eq!(parse_duration("30").unwrap(), Duration::from_secs(30));
    }

    #[test]
    fn parse_duration_rejects_garbage() {
        assert!(parse_duration("").is_err());
        assert!(parse_duration("banana").is_err());
        assert!(parse_duration("10x").is_err());
        assert!(parse_duration("0s").is_err());
        assert!(parse_duration("0").is_err());
        assert!(parse_duration("-5s").is_err());
    }

    #[test]
    fn format_duration_round_trips() {
        for text in ["500ms", "10s", "2m", "1h", "90s", "1500ms"] {
            let parsed = parse_duration(text).unwrap();
            let rendered = format_duration(&parsed);
            assert_eq!(
                parse_duration(&rendered).unwrap(),
                parsed,
                "round-trip failed for {text}"
            );
        }
        assert_eq!(format_duration(&parse_duration("90s").unwrap()), "90s");
        assert_eq!(format_duration(&parse_duration("2m").unwrap()), "2m");
    }

    #[test]
    fn structural_metadata_covers_all_structural_kinds() {
        use crate::ast::Value;
        use std::time::Duration;

        // Tripwire: adding a structural StepKind variant without registering
        // documentation fails to compile here (non-exhaustive match). Leaf
        // commands map to None; they are covered by declare_commands!.
        fn metadata_name(kind: &StepKind) -> Option<&'static str> {
            match kind {
                StepKind::WithIo { .. } | StepKind::WithIoBlock { .. } => Some("WITH_IO"),
                StepKind::For { .. } => Some("FOR"),
                StepKind::If { .. } => Some("IF"),
                StepKind::Assign { .. } => Some("LET"),
                StepKind::AsyncBlock { .. } | StepKind::AssignAsync { .. } => Some("ASYNC"),
                StepKind::Await { .. } => Some("AWAIT"),
                StepKind::Cancel { .. } => Some("CANCEL"),
                StepKind::Timeout { .. } => Some("TIMEOUT"),
                StepKind::Workdir(_)
                | StepKind::Workspace(_)
                | StepKind::Env { .. }
                | StepKind::InheritEnv { .. }
                | StepKind::Run(_)
                | StepKind::Echo(_)
                | StepKind::Copy { .. }
                | StepKind::Symlink { .. }
                | StepKind::Mkdir(_)
                | StepKind::Ls(_)
                | StepKind::Cwd
                | StepKind::Read(_)
                | StepKind::ReadLine { .. }
                | StepKind::Write { .. }
                | StepKind::Append { .. }
                | StepKind::Expand { .. }
                | StepKind::AssertFile { .. }
                | StepKind::AssertDir(_)
                | StepKind::AssertAbsent(_)
                | StepKind::AssertStdout(_)
                | StepKind::CopyGit { .. }
                | StepKind::HashSha256 { .. }
                | StepKind::Exit(_)
                | StepKind::Sleep { .. } => None,
            }
        }

        // Exercise the matcher once per structural variant so the arms cannot
        // rot (a new variant breaks compilation above first).
        let dummies: Vec<StepKind> = vec![
            StepKind::WithIo {
                bindings: Vec::new(),
                cmd: Box::new(StepKind::Echo(crate::ast::Arg::String(
                    "x".to_string(),
                    false,
                ))),
            },
            StepKind::For {
                key_var: None,
                var: "i".to_string(),
                in_expr: Expr::Literal(Value::Bool(true)),
                body: Vec::new(),
            },
            StepKind::If {
                cond: Box::new(Expr::Literal(Value::Bool(true))),
                then_body: Vec::new(),
                else_ifs: Vec::new(),
                else_body: None,
            },
            StepKind::Assign {
                var: "v".to_string(),
                expr: Expr::Literal(Value::Bool(true)),
            },
            StepKind::AsyncBlock { body: Vec::new() },
            StepKind::AssignAsync {
                var: "t".to_string(),
                body: Vec::new(),
            },
            StepKind::Await {
                var: "t".to_string(),
            },
            StepKind::Cancel {
                var: "t".to_string(),
            },
            StepKind::Timeout {
                duration: Duration::from_secs(1),
                body: Vec::new(),
            },
        ];
        let registry = all_structural_metadata();
        for kind in &dummies {
            let name = metadata_name(kind).expect("structural kind must map to metadata");
            assert!(
                registry.iter().any(|meta| meta.name == name),
                "no structural metadata entry for {}",
                name
            );
        }
    }

    #[test]
    fn verify_display_sync_with_metadata() {
        let registry = all_metadata();
        for meta in registry {
            if meta.examples.is_empty() {
                continue;
            }

            let code = meta.examples[0].code;
            let ast = parse_script(code, lower_command)
                .unwrap_or_else(|e| panic!("Failed to parse example for {}: {}", meta.name, e));

            let matching = ast.iter().find(|step| {
                let kind = match &step.kind {
                    StepKind::WithIo { cmd, .. } => &**cmd,
                    other => other,
                };
                // Full Display covers wrapper kinds themselves (e.g. a
                // WithIo step displays as WITH_IO ...); unwrapped covers
                // wrapped leaf commands.
                kind.to_string().starts_with(meta.name)
                    || step.kind.to_string().starts_with(meta.name)
            });

            assert!(
                matching.is_some(),
                "No step in example for {} produces Display starting with {}",
                meta.name,
                meta.name
            );
        }
    }
}
