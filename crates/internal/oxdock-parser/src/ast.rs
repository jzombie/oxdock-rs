use std::collections::HashMap;

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

    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "WORKDIR" => Some(Command::Workdir),
            "WORKSPACE" => Some(Command::Workspace),
            "ENV" => Some(Command::Env),
            "ECHO" => Some(Command::Echo),
            "RUN" => Some(Command::Run),
            "RUN_BG" => Some(Command::RunBg),
            "COPY" => Some(Command::Copy),
            "CAPTURE" => Some(Command::Capture),
            "COPY_GIT" => Some(Command::CopyGit),
            "SYMLINK" => Some(Command::Symlink),
            "MKDIR" => Some(Command::Mkdir),
            "LS" => Some(Command::Ls),
            "CWD" => Some(Command::Cwd),
            "CAT" => Some(Command::Cat),
            "WRITE" => Some(Command::Write),
            "EXIT" => Some(Command::Exit),
            _ => None,
        }
    }
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

fn platform_matches(target: PlatformGuard) -> bool {
    #[allow(clippy::disallowed_macros)]
    match target {
        PlatformGuard::Unix => cfg!(unix),
        PlatformGuard::Windows => cfg!(windows),
        PlatformGuard::Macos => cfg!(target_os = "macos"),
        PlatformGuard::Linux => cfg!(target_os = "linux"),
    }
}

pub fn guard_allows(guard: &Guard, script_envs: &HashMap<String, String>) -> bool {
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

pub fn guard_group_allows(group: &[Guard], script_envs: &HashMap<String, String>) -> bool {
    group.iter().all(|g| guard_allows(g, script_envs))
}

pub fn guards_allow_any(groups: &[Vec<Guard>], script_envs: &HashMap<String, String>) -> bool {
    if groups.is_empty() {
        return true;
    }
    groups.iter().any(|g| guard_group_allows(g, script_envs))
}

use std::fmt;

impl fmt::Display for PlatformGuard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlatformGuard::Unix => write!(f, "unix"),
            PlatformGuard::Windows => write!(f, "windows"),
            PlatformGuard::Macos => write!(f, "macos"),
            PlatformGuard::Linux => write!(f, "linux"),
        }
    }
}

impl fmt::Display for Guard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Guard::Platform { target, invert } => {
                if *invert {
                    write!(f, "!")?
                }
                write!(f, "platform:{}", target)
            }
            Guard::EnvExists { key, invert } => {
                if *invert {
                    write!(f, "!")?
                }
                write!(f, "env:{}", key)
            }
            Guard::EnvEquals { key, value, invert } => {
                if *invert {
                    write!(f, "!")?
                }
                write!(f, "env:{}={}", key, value)
            }
        }
    }
}

impl fmt::Display for WorkspaceTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WorkspaceTarget::Snapshot => write!(f, "SNAPSHOT"),
            WorkspaceTarget::Local => write!(f, "LOCAL"),
        }
    }
}

fn quote_arg(s: &str) -> String {
    // Strict quoting to avoid parser ambiguity, especially with CAPTURE command
    // where unquoted args followed by run_args can be consumed greedily.
    let is_safe = s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_');
    if is_safe && !s.is_empty() {
        s.to_string()
    } else {
        format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
    }
}

fn quote_msg(s: &str) -> String {
    // Strict quoting to ensure round-trip stability through TokenStream (macro input).
    // The macro input reconstructor removes spaces around "sticky" characters (/-.:=)
    // and collapses multiple spaces, so we must quote strings containing them.
    // We also quote strings with spaces to be safe, as TokenStream does not preserve whitespace.
    let is_safe = s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_');

    if is_safe && !s.is_empty() {
        s.to_string()
    } else {
        format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
    }
}

fn quote_run(s: &str) -> String {
    // For RUN commands, we want to preserve the raw string as much as possible
    // because the parser now preserves quotes.
    // We only quote if the string contains characters that would break the parser
    // (semicolon, newline, comments) or if it's empty.
    // We also quote if it starts with a "sticky" character (/-.:=) or contains whitespace
    // to ensure proper spacing when round-tripping through TokenStream (macro input).
    let needs_quote = s.is_empty()
        || s.chars()
            .any(|c| c.is_whitespace() || c == ';' || c == '\n' || c == '\r')
        || s.contains("//")
        || s.contains("/*")
        || s.starts_with(|c| matches!(c, '/' | '.' | '-' | ':' | '='));

    if !needs_quote {
        s.to_string()
    } else {
        format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
    }
}

impl fmt::Display for StepKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StepKind::Workdir(arg) => write!(f, "WORKDIR {}", quote_arg(arg)),
            StepKind::Workspace(target) => write!(f, "WORKSPACE {}", target),
            StepKind::Env { key, value } => write!(f, "ENV {}={}", key, quote_arg(value)),
            StepKind::Run(cmd) => write!(f, "RUN {}", quote_run(cmd)),
            StepKind::Echo(msg) => write!(f, "ECHO {}", quote_msg(msg)),
            StepKind::RunBg(cmd) => write!(f, "RUN_BG {}", quote_run(cmd)),
            StepKind::Copy { from, to } => write!(f, "COPY {} {}", quote_arg(from), quote_arg(to)),
            StepKind::Symlink { from, to } => {
                write!(f, "SYMLINK {} {}", quote_arg(from), quote_arg(to))
            }
            StepKind::Mkdir(arg) => write!(f, "MKDIR {}", quote_arg(arg)),
            StepKind::Ls(arg) => {
                write!(f, "LS")?;
                if let Some(a) = arg {
                    write!(f, " {}", quote_arg(a))?;
                }
                Ok(())
            }
            StepKind::Cwd => write!(f, "CWD"),
            StepKind::Cat(arg) => write!(f, "CAT {}", quote_arg(arg)),
            StepKind::Write { path, contents } => {
                write!(f, "WRITE {} {}", quote_arg(path), quote_msg(contents))
            }
            StepKind::Capture { path, cmd } => {
                write!(f, "CAPTURE {} {}", quote_arg(path), quote_run(cmd))
            }
            StepKind::CopyGit { rev, from, to } => write!(
                f,
                "COPY_GIT {} {} {}",
                quote_arg(rev),
                quote_arg(from),
                quote_arg(to)
            ),
            StepKind::Exit(code) => write!(f, "EXIT {}", code),
        }
    }
}

impl fmt::Display for Step {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for group in &self.guards {
            write!(f, "[")?;
            for (i, guard) in group.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?
                }
                write!(f, "{}", guard)?;
            }
            write!(f, "] ")?;
        }
        write!(f, "{}", self.kind)
    }
}
