use anyhow::{Result, anyhow, bail};
use std::collections::{HashMap, VecDeque};

mod lexer;
pub use lexer::LANGUAGE_SPEC;
use lexer::{GuardToken, LexedCommand, Token};

#[cfg(feature = "token-input")]
mod macro_input;
#[cfg(feature = "token-input")]
pub use macro_input::parse_braced_tokens;
#[cfg(feature = "token-input")]
pub use macro_input::{DslMacroInput, ScriptSource, script_from_braced_tokens};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Command {
    Workdir,
    Workspace,
    Env,
    Echo,
    Run,
    RunBg,
    Copy,
    Capture,
    CopyGit,
    Symlink,
    Mkdir,
    Ls,
    Cwd,
    Cat,
    Write,
    Exit,
}

pub const COMMANDS: &[Command] = &[
    Command::Workdir,
    Command::Workspace,
    Command::Env,
    Command::Echo,
    Command::Run,
    Command::RunBg,
    Command::Copy,
    Command::Capture,
    Command::CopyGit,
    Command::Symlink,
    Command::Mkdir,
    Command::Ls,
    Command::Cwd,
    Command::Cat,
    Command::Write,
    Command::Exit,
];

fn platform_matches(target: PlatformGuard) -> bool {
    #[allow(clippy::disallowed_macros)]
    match target {
        PlatformGuard::Unix => cfg!(unix),
        PlatformGuard::Windows => cfg!(windows),
        PlatformGuard::Macos => cfg!(target_os = "macos"),
        PlatformGuard::Linux => cfg!(target_os = "linux"),
    }
}

fn guard_allows(guard: &Guard, script_envs: &HashMap<String, String>) -> bool {
    match guard {
        Guard::Platform { target, invert } => {
            let res = platform_matches(*target);
            if *invert { !res } else { res }
        }
        Guard::EnvExists { key, invert } => {
            let res = script_envs
                .get(key)
                .cloned()
                .or_else(|| std::env::var(key).ok())
                .map(|v| !v.is_empty())
                .unwrap_or(false);
            if *invert { !res } else { res }
        }
        Guard::EnvEquals { key, value, invert } => {
            let res = script_envs
                .get(key)
                .cloned()
                .or_else(|| std::env::var(key).ok())
                .map(|v| v == *value)
                .unwrap_or(false);
            if *invert { !res } else { res }
        }
    }
}

fn guard_group_allows(group: &[Guard], script_envs: &HashMap<String, String>) -> bool {
    group.iter().all(|g| guard_allows(g, script_envs))
}

pub fn guards_allow_any(groups: &[Vec<Guard>], script_envs: &HashMap<String, String>) -> bool {
    if groups.is_empty() {
        return true;
    }
    groups.iter().any(|g| guard_group_allows(g, script_envs))
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PlatformGuard {
    Unix,
    Windows,
    Macos,
    Linux,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Guard {
    Platform {
        target: PlatformGuard,
        invert: bool,
    },
    EnvExists {
        key: String,
        invert: bool,
    },
    EnvEquals {
        key: String,
        value: String,
        invert: bool,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StepKind {
    Workdir(String),
    Workspace(WorkspaceTarget),
    Env {
        key: String,
        value: String,
    },
    Run(String),
    Echo(String),
    RunBg(String),
    Copy {
        from: String,
        to: String,
    },
    Symlink {
        from: String,
        to: String,
    },
    Mkdir(String),
    Ls(Option<String>),
    Cwd,
    Cat(String),
    Write {
        path: String,
        contents: String,
    },
    Capture {
        path: String,
        cmd: String,
    },
    CopyGit {
        rev: String,
        from: String,
        to: String,
    },
    Exit(i32),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Step {
    pub guards: Vec<Vec<Guard>>,
    pub kind: StepKind,
    pub scope_enter: usize,
    pub scope_exit: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum WorkspaceTarget {
    Snapshot,
    Local,
}


fn combine_guard_groups(a: &[Vec<Guard>], b: &[Vec<Guard>]) -> Vec<Vec<Guard>> {
    if a.is_empty() {
        return b.to_vec();
    }
    if b.is_empty() {
        return a.to_vec();
    }
    let mut combined = Vec::new();
    for left in a {
        for right in b {
            let mut merged = left.clone();
            merged.extend(right.clone());
            combined.push(merged);
        }
    }
    combined
}

#[derive(Clone)]
struct ScopeFrame {
    line_no: usize,
    had_command: bool,
}

struct ScriptParser {
    tokens: VecDeque<Token>,
    steps: Vec<Step>,
    guard_stack: Vec<Vec<Vec<Guard>>>,
    pending_guards: Option<Vec<Vec<Guard>>>,
    pending_inline_guards: Option<Vec<Vec<Guard>>>,
    pending_can_open_block: bool,
    pending_scope_enters: usize,
    scope_stack: Vec<ScopeFrame>,
}

impl ScriptParser {
    fn new(input: &str) -> Result<Self> {
        let tokens = VecDeque::from(lexer::lex_script(input)?);
        Ok(Self {
            tokens,
            steps: Vec::new(),
            guard_stack: vec![Vec::new()],
            pending_guards: None,
            pending_inline_guards: None,
            pending_can_open_block: false,
            pending_scope_enters: 0,
            scope_stack: Vec::new(),
        })
    }

    fn parse(mut self) -> Result<Vec<Step>> {
        while let Some(token) = self.tokens.pop_front() {
            match token {
                Token::Guard(guard) => self.handle_guard_token(guard)?,
                Token::BlockStart { line_no } => self.start_block_from_pending(line_no)?,
                Token::BlockEnd { line_no } => self.end_block(line_no)?,
                Token::Command(cmd) => self.handle_command_token(cmd)?,
            }
        }

        if self.guard_stack.len() != 1 {
            bail!("unclosed guard block at end of script");
        }
        if let Some(pending) = &self.pending_guards
            && !pending.is_empty()
        {
            bail!("guard declared on final lines without a following command");
        }

        Ok(self.steps)
    }

    fn handle_guard_token(&mut self, guard: GuardToken) -> Result<()> {
        if let Some(Token::Command(cmd)) = self.tokens.front() {
            if cmd.line_no == guard.line_end {
                self.pending_inline_guards = Some(guard.groups);
                self.pending_can_open_block = false;
                return Ok(());
            }
        }
        self.stash_pending_guard(guard.groups);
        self.pending_can_open_block = true;
        Ok(())
    }

    fn handle_command_token(&mut self, token: LexedCommand) -> Result<()> {
        let inline = self.pending_inline_guards.take();
        self.handle_command(token.line_no, token.kind, inline)
    }

    fn stash_pending_guard(&mut self, groups: Vec<Vec<Guard>>) {
        self.pending_guards = Some(if let Some(existing) = self.pending_guards.take() {
            combine_guard_groups(&existing, &groups)
        } else {
            groups
        });
    }

    fn start_block_from_pending(&mut self, line_no: usize) -> Result<()> {
        let guards = self
            .pending_guards
            .take()
            .ok_or_else(|| anyhow!("line {}: '{{' without a pending guard", line_no))?;
        if !self.pending_can_open_block {
            bail!("line {}: '{{' must directly follow a guard", line_no);
        }
        self.pending_can_open_block = false;
        self.start_block(guards, line_no)
    }

    fn start_block(&mut self, guards: Vec<Vec<Guard>>, line_no: usize) -> Result<()> {
        let with_pending = if let Some(pending) = self.pending_guards.take() {
            combine_guard_groups(&pending, &guards)
        } else {
            guards
        };
        let parent = self.guard_stack.last().cloned().unwrap_or_default();
        let next = if parent.is_empty() {
            with_pending
        } else if with_pending.is_empty() {
            parent
        } else {
            combine_guard_groups(&parent, &with_pending)
        };
        self.guard_stack.push(next);
        self.scope_stack.push(ScopeFrame {
            line_no,
            had_command: false,
        });
        self.pending_scope_enters += 1;
        Ok(())
    }

    fn end_block(&mut self, line_no: usize) -> Result<()> {
        if self.guard_stack.len() == 1 {
            bail!("line {}: unexpected '}}'", line_no);
        }
        if self.pending_guards.is_some() {
            bail!(
                "line {}: guard declared immediately before '}}' without a command",
                line_no
            );
        }
        let frame = self
            .scope_stack
            .last()
            .cloned()
            .ok_or_else(|| anyhow!("line {}: scope stack underflow", line_no))?;
        if !frame.had_command {
            bail!(
                "line {}: guard block starting on line {} must contain at least one command",
                line_no,
                frame.line_no
            );
        }
        let step = self
            .steps
            .last_mut()
            .ok_or_else(|| anyhow!("line {}: guard block closed without any commands", line_no))?;
        step.scope_exit += 1;
        self.scope_stack.pop();
        self.guard_stack.pop();
        Ok(())
    }

    fn guard_context(&mut self, inline: Option<Vec<Vec<Guard>>>) -> Vec<Vec<Guard>> {
        let mut context = if let Some(top) = self.guard_stack.last() {
            top.clone()
        } else {
            Vec::new()
        };
        if let Some(pending) = self.pending_guards.take() {
            context = if context.is_empty() {
                pending
            } else {
                combine_guard_groups(&context, &pending)
            };
            self.pending_can_open_block = false;
        }
        if let Some(inline_groups) = inline {
            context = if context.is_empty() {
                inline_groups
            } else {
                combine_guard_groups(&context, &inline_groups)
            };
        }
        context
    }

    fn handle_command(
        &mut self,
        _line_no: usize,
        kind: StepKind,
        inline_guards: Option<Vec<Vec<Guard>>>,
    ) -> Result<()> {
        let guards = self.guard_context(inline_guards);
        let scope_enter = self.pending_scope_enters;
        self.pending_scope_enters = 0;
        for frame in self.scope_stack.iter_mut() {
            frame.had_command = true;
        }
        self.steps.push(Step {
            guards,
            kind,
            scope_enter,
            scope_exit: 0,
        });
        Ok(())
    }
}

pub fn parse_script(input: &str) -> Result<Vec<Step>> {
    ScriptParser::new(input)?.parse()
}

impl Command {
    pub const fn as_str(self) -> &'static str {
        match self {
            Command::Workdir => "WORKDIR",
            Command::Workspace => "WORKSPACE",
            Command::Env => "ENV",
            Command::Echo => "ECHO",
            Command::Run => "RUN",
            Command::RunBg => "RUN_BG",
            Command::Copy => "COPY",
            Command::Capture => "CAPTURE",
            Command::CopyGit => "COPY_GIT",
            Command::Symlink => "SYMLINK",
            Command::Mkdir => "MKDIR",
            Command::Ls => "LS",
            Command::Cwd => "CWD",
            Command::Cat => "CAT",
            Command::Write => "WRITE",
            Command::Exit => "EXIT",
        }
    }

    pub fn parse(op: &str) -> Option<Self> {
        COMMANDS.iter().copied().find(|c| c.as_str() == op)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn commands_are_case_sensitive() {
        for bad in ["run echo hi", "Run echo hi", "rUn echo hi", "write foo bar"] {
            parse_script(bad).expect_err("mixed/lowercase commands must fail");
        }
    }

    #[test]
    fn string_dsl_supports_rust_style_comments() {
        let script = indoc! {r#"
            // leading comment line
            WORKDIR /tmp // inline comment
            RUN echo "keep // literal"
            /* block comment
               WORKDIR ignored
               /* nested inner */
               RUN ignored as well
            */
            RUN echo final
            RUN echo 'literal /* stay */ value'
        "#};
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 4, "expected 4 executable steps");
        match &steps[0].kind {
            StepKind::Workdir(path) => assert_eq!(path, "/tmp"),
            other => panic!("expected WORKDIR, saw {:?}", other),
        }
        match &steps[1].kind {
            StepKind::Run(cmd) => assert_eq!(cmd, "echo \"keep // literal\""),
            other => panic!("expected RUN, saw {:?}", other),
        }
        match &steps[2].kind {
            StepKind::Run(cmd) => assert_eq!(cmd, "echo final"),
            other => panic!("expected RUN, saw {:?}", other),
        }
        match &steps[3].kind {
            StepKind::Run(cmd) => assert_eq!(cmd, "echo 'literal /* stay */ value'"),
            other => panic!("expected RUN, saw {:?}", other),
        }
    }

    #[test]
    fn semicolon_attached_to_command_splits_instructions() {
        let script = "LS;LS;LS";
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 3);
        assert!(
            steps
                .iter()
                .all(|step| matches!(step.kind, StepKind::Ls(_)))
        );
    }

    #[test]
    fn string_dsl_errors_on_unclosed_block_comment() {
        let err = parse_script("RUN echo hi /*").expect_err("unclosed block comment should error");
        assert!(
            err.to_string().contains("expected"),
            "unexpected error message: {err}"
        );
    }

    #[cfg(feature = "token-input")]
    #[test]
    fn string_and_braced_scripts_produce_identical_ast() {
        use quote::quote;

        let atomic_instructions: Vec<(&str, proc_macro2::TokenStream)> = vec![
            ("WORKDIR /tmp", quote! { WORKDIR /tmp }),
            ("ENV FOO=bar", quote! { ENV FOO=bar }),
            ("RUN echo hi && ls", quote! { RUN echo hi && ls }),
            ("WRITE dist/out.txt hi", quote! { WRITE dist/out.txt "hi" }),
            (
                "COPY src/file dist/file",
                quote! { COPY src/file dist/file },
            ),
        ];

        let mut cases: Vec<(String, proc_macro2::TokenStream)> = Vec::new();

        for mask in 1..(1 << atomic_instructions.len()) {
            let mut literal_parts = Vec::new();
            let mut token_parts = Vec::new();
            for (idx, (lit, tokens)) in atomic_instructions.iter().enumerate() {
                if (mask & (1 << idx)) != 0 {
                    literal_parts.push(*lit);
                    token_parts.push(tokens.clone());
                }
            }
            let literal = literal_parts.join("\n");
            let tokens = quote! { #(#token_parts)* };
            cases.push((literal, tokens));
        }

        cases.push((
            indoc! {r#"
                [env:PROFILE=release]
                RUN echo release
                RUN echo done
            "#}
            .trim()
            .to_string(),
            quote! {
                [env:PROFILE=release] RUN echo release
                RUN echo done
            },
        ));

        cases.push((
            indoc! {r#"
                [platform:linux] {
                    WORKDIR /client
                    RUN echo linux
                }
                [env:FOO=bar] {
                    WRITE scoped.txt hit
                }
                RUN echo finished
            "#}
            .trim()
            .to_string(),
            quote! {
                [platform:linux] {
                    WORKDIR /client
                    RUN echo linux
                }
                [env:FOO=bar] {
                    WRITE scoped.txt hit
                }
                RUN echo finished
            },
        ));

        cases.push((
            indoc! {r#"
                [!env:SKIP]
                [platform:windows] RUN echo win
                [ env:MODE=beta,
                  linux
                ] RUN echo combo
            "#}
            .trim()
            .to_string(),
            quote! {
                [!env:SKIP]
                [platform:windows] RUN echo win
                [env:MODE=beta, linux] RUN echo combo
            },
        ));

        cases.push((
            indoc! {r#"
                [env:OUTER] {
                    WORKDIR /tmp
                    [env:INNER] {
                        RUN echo inner; echo still
                    }
                    WRITE after.txt ok
                }
                RUN echo done
            "#}
            .trim()
            .to_string(),
            quote! {
                [env:OUTER] {
                    WORKDIR /tmp
                    [env:INNER] {
                        RUN echo inner; echo still
                    }
                    WRITE after.txt ok
                }
                RUN echo done
            },
        ));

        cases.push((
            indoc! {r#"
                [env:TEST=1] CAPTURE out.txt RUN echo hi
                [env:FOO] WRITE foo.txt bar
                SYMLINK link target
            "#}
            .trim()
            .to_string(),
            quote! {
                [env:TEST=1] CAPTURE out.txt RUN echo hi
                [env:FOO] WRITE foo.txt "bar"
                SYMLINK link target
            },
        ));

        for (idx, (literal, tokens)) in cases.iter().enumerate() {
            let text = literal.trim();
            let string_steps = parse_script(text)
                .unwrap_or_else(|e| panic!("string parse failed for case {idx}: {e}"));
            let braced_steps = parse_braced_tokens(tokens)
                .unwrap_or_else(|e| panic!("token parse failed for case {idx}: {e}"));
            assert_eq!(
                string_steps, braced_steps,
                "AST mismatch for case {idx} literal:\n{text}"
            );
        }
    }

    #[test]
    fn env_equals_guard_respects_inversion() {
        let mut envs = HashMap::new();
        envs.insert("FOO".to_string(), "bar".to_string());
        let guard = Guard::EnvEquals {
            key: "FOO".into(),
            value: "bar".into(),
            invert: false,
        };
        assert!(guard_allows(&guard, &envs));

        let inverted = Guard::EnvEquals {
            key: "FOO".into(),
            value: "bar".into(),
            invert: true,
        };
        assert!(!guard_allows(&inverted, &envs));
    }

    #[test]
    fn guards_allow_any_act_as_or_of_ands() {
        let mut envs = HashMap::new();
        envs.insert("MODE".to_string(), "beta".to_string());
        let groups = vec![
            vec![Guard::EnvEquals {
                key: "MODE".into(),
                value: "alpha".into(),
                invert: false,
            }],
            vec![Guard::EnvEquals {
                key: "MODE".into(),
                value: "beta".into(),
                invert: false,
            }],
        ];
        assert!(guards_allow_any(&groups, &envs));
    }

    #[test]
    fn guards_allow_any_falls_back_to_false_when_all_fail() {
        let envs = HashMap::new();
        let groups = vec![vec![Guard::EnvExists {
            key: "MISSING".into(),
            invert: false,
        }]];
        assert!(!guards_allow_any(&groups, &envs));
    }

    #[test]
    fn multi_line_guard_blocks_apply_to_next_command() {
        let script = indoc! {r#"
            [ env:FOO=bar,
              linux
            ]
            RUN echo guarded
        "#};
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 1);
        assert_eq!(steps[0].guards.len(), 1);
        assert!(matches!(steps[0].kind, StepKind::Run(ref cmd) if cmd == "echo guarded"));
    }

    #[test]
    fn guarded_brace_blocks_apply_to_all_inner_steps() {
        let script = indoc! {r#"
            [env:APP=demo] {
                WRITE one.txt 1
                WRITE two.txt 2
            }
            WRITE three.txt 3
        "#};
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 3);
        assert_eq!(steps[0].guards.len(), 1);
        assert_eq!(steps[1].guards.len(), 1);
        assert!(steps[2].guards.is_empty());
    }

    #[test]
    fn nested_guard_blocks_stack() {
        let script = indoc! {r#"
            [env:OUTER] {
                [env:INNER] {
                    WRITE nested.txt yes
                }
            }
        "#};
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 1);
        assert_eq!(steps[0].guards.len(), 1);
        assert_eq!(steps[0].guards[0].len(), 2);
    }

    #[test]
    fn brace_blocks_require_guard() {
        let script = indoc! {r#"
            {
                WRITE nope.txt hi
            }
        "#};
        let err = parse_script(script).expect_err("block without guard must fail");
        assert!(err.to_string().contains("'{'"), "unexpected error: {err}");
    }

    #[test]
    fn guard_lines_chain_before_block() {
        let script = indoc! {r#"
            [env:FOO]
            [linux]
            {
                WRITE ok.txt hi
            }
        "#};
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 1);
        assert_eq!(steps[0].guards[0].len(), 2);
    }

    #[test]
    fn guard_block_emits_scope_markers() {
        let script = indoc! {r#"
            ENV RUN=1
            [env:RUN] {
                WRITE one.txt 1
                WRITE two.txt 2
            }
            WRITE three.txt 3
        "#};
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 4);
        assert_eq!(steps[1].scope_enter, 1);
        assert_eq!(steps[1].scope_exit, 0);
        assert_eq!(steps[2].scope_enter, 0);
        assert_eq!(steps[2].scope_exit, 1);
        assert_eq!(steps[3].scope_enter, 0);
        assert_eq!(steps[3].scope_exit, 0);
    }

    #[test]
    fn nested_guard_block_scopes_stack_counts() {
        let script = indoc! {r#"
            [env:OUTER] {
                [env:INNER] {
                    WRITE deep.txt ok
                }
            }
        "#};
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 1);
        assert_eq!(steps[0].scope_enter, 2);
        assert_eq!(steps[0].scope_exit, 2);
    }

    #[test]
    fn guard_block_must_contain_command() {
        let script = indoc! {r#"
            [env:FOO]
            {
            }
        "#};
        let err = parse_script(script).expect_err("empty block must fail");
        assert!(
            err.to_string()
                .contains("must contain at least one command"),
            "unexpected error: {err}"
        );
    }
}
