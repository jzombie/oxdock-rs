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
    CaptureToFile,
    WithIo,
    CopyGit,
    HashSha256,
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
    Command::CaptureToFile,
    Command::WithIo,
    Command::CopyGit,
    Command::HashSha256,
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
            Command::CaptureToFile => "CAPTURE_TO_FILE",
            Command::WithIo => "WITH_IO",
            Command::CopyGit => "COPY_GIT",
            Command::HashSha256 => "HASH_SHA256",
            Command::Symlink => "SYMLINK",
            Command::Mkdir => "MKDIR",
            Command::Ls => "LS",
            Command::Cwd => "CWD",
            Command::Cat => "CAT",
            Command::Write => "WRITE",
            Command::Exit => "EXIT",
        }
    }

    pub const fn expects_inner_command(self) -> bool {
        matches!(self, Command::CaptureToFile | Command::WithIo)
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
            "CAPTURE_TO_FILE" => Some(Command::CaptureToFile),
            "WITH_IO" => Some(Command::WithIo),
            "COPY_GIT" => Some(Command::CopyGit),
            "HASH_SHA256" => Some(Command::HashSha256),
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
pub struct TemplateString(pub String);

impl From<String> for TemplateString {
    fn from(s: String) -> Self {
        TemplateString(s)
    }
}

impl From<&str> for TemplateString {
    fn from(s: &str) -> Self {
        TemplateString(s.to_string())
    }
}

impl std::fmt::Display for TemplateString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for TemplateString {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl PartialEq<str> for TemplateString {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl PartialEq<&str> for TemplateString {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}

impl std::ops::Deref for TemplateString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StepKind {
    Workdir(TemplateString),
    Workspace(WorkspaceTarget),
    Env {
        key: String,
        value: TemplateString,
    },
    Run(TemplateString),
    Echo(TemplateString),
    RunBg(TemplateString),
    Copy {
        from: TemplateString,
        to: TemplateString,
    },
    Symlink {
        from: TemplateString,
        to: TemplateString,
    },
    Mkdir(TemplateString),
    Ls(Option<TemplateString>),
    Cwd,
    Cat(Option<TemplateString>),
    Write {
        path: TemplateString,
        contents: TemplateString,
    },
    CaptureToFile {
        path: TemplateString,
        cmd: Box<StepKind>,
    },
    WithIo {
        streams: Vec<String>,
        cmd: Box<StepKind>,
    },
    CopyGit {
        rev: TemplateString,
        from: TemplateString,
        to: TemplateString,
        include_dirty: bool,
    },
    HashSha256 {
        path: TemplateString,
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
                    write!(f, "!platform:{}", target)
                } else {
                    write!(f, "platform:{}", target)
                }
            }
            Guard::EnvExists { key, invert } => {
                if *invert {
                    write!(f, "!")?
                }
                write!(f, "env:{}", key)
            }
            Guard::EnvEquals { key, value, invert } => {
                if *invert {
                    write!(f, "env:{}!={}", key, value)
                } else {
                    write!(f, "env:{}=={}", key, value)
                }
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
    // Strict quoting to avoid parser ambiguity, especially with CAPTURE_TO_FILE command
    // where unquoted args followed by run_args can be consumed greedily.
    // Also quote if it starts with a digit to avoid invalid Rust tokens (e.g. 0o8) in macros.
    let is_safe = s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
        && !s.starts_with(|c: char| c.is_ascii_digit());
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
    // Also quote if it starts with a digit to avoid invalid Rust tokens.
    let is_safe = s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
        && !s.starts_with(|c: char| c.is_ascii_digit());

    if is_safe && !s.is_empty() {
        s.to_string()
    } else {
        format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
    }
}

fn quote_run(s: &str) -> String {
    // For RUN commands, we want to preserve the raw string as much as possible.
    // However, to ensure round-trip stability through TokenStream (macro input),
    // we must ensure that the generated string is a valid sequence of Rust tokens.
    // Invalid tokens (like 0o8) must be quoted.
    // Also, sticky characters (like -) can merge with previous tokens in macro input,
    // so we quote words starting with them to ensure separation.

    let force_full_quote = s.is_empty()
        || s.chars().any(|c| c == ';' || c == '\n' || c == '\r')
        || s.contains("//")
        || s.contains("/*");

    if force_full_quote {
        return format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""));
    }

    s.split(' ')
        .map(|word| {
            let needs_quote = word.starts_with(|c: char| c.is_ascii_digit())
                || word.starts_with(['/', '.', '-', ':', '=']);
            if needs_quote {
                format!("\"{}\"", word.replace('\\', "\\\\").replace('"', "\\\""))
            } else {
                word.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
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
            StepKind::Cat(arg) => {
                write!(f, "CAT")?;
                if let Some(a) = arg {
                    write!(f, " {}", quote_arg(a))?;
                }
                Ok(())
            }
            StepKind::Write { path, contents } => {
                write!(f, "WRITE {} {}", quote_arg(path), quote_msg(contents))
            }
            StepKind::CaptureToFile { path, cmd } => {
                write!(f, "CAPTURE_TO_FILE {} {}", quote_arg(path), cmd)
            }
            StepKind::WithIo { streams, cmd } => {
                write!(f, "WITH_IO [{}] {}", streams.join(", "), cmd)
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
                        quote_arg(rev),
                        quote_arg(from),
                        quote_arg(to)
                    )
                } else {
                    write!(
                        f,
                        "COPY_GIT {} {} {}",
                        quote_arg(rev),
                        quote_arg(from),
                        quote_arg(to)
                    )
                }
            }
            StepKind::HashSha256 { path } => write!(f, "HASH_SHA256 {}", quote_arg(path)),
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
