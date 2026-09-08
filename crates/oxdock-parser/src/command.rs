use crate::ast::{Arg, StepKind};
use anyhow::Result;

/// Metadata for a single command argument.
pub struct ArgSpec {
    pub name: &'static str,
    pub arg_type: ArgType,
    pub description: &'static str,
    pub io: IoDirection,
    pub index: usize,
    pub required: bool,
    pub fallback_stream: Option<Stream>,
}

/// Closed vocabulary for argument value types.
///
/// The closed enum keeps the vocabulary compiler-checked and lets
/// docs-gen link each type cell to its reference section instead of
/// printing bare words like `duration` with no explanation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgType {
    String,
    Path,
    Int,
    Duration,
    Var,
    KeyValue,
    /// Inline alternation for one-off enums (e.g. `SNAPSHOT|LOCAL`).
    /// Self-describing, so it renders unlinked.
    OneOf(&'static [&'static str]),
    /// Trailing variadic repetition (e.g. `RUN`'s `string...`).
    Rest(&'static ArgType),
}

impl ArgType {
    /// The documented types, in reference-section order.
    pub const CANONICAL: &[ArgType] = &[
        ArgType::String,
        ArgType::Path,
        ArgType::Int,
        ArgType::Duration,
        ArgType::Var,
        ArgType::KeyValue,
    ];

    /// Table-cell label, e.g. `duration` or `SNAPSHOT|LOCAL`.
    pub fn label(&self) -> String {
        match self {
            ArgType::String => "string".to_string(),
            ArgType::Path => "path".to_string(),
            ArgType::Int => "int".to_string(),
            ArgType::Duration => "duration".to_string(),
            ArgType::Var => "$var".to_string(),
            ArgType::KeyValue => "KEY=value".to_string(),
            ArgType::OneOf(options) => options.join("|"),
            ArgType::Rest(inner) => format!("{}...", inner.label()),
        }
    }

    /// Anchor of the type's reference section (`### Value type: <label>`),
    /// or `None` for self-describing inline alternations.
    pub fn anchor(&self) -> Option<&'static str> {
        match self {
            ArgType::String => Some("value-type-string"),
            ArgType::Path => Some("value-type-path"),
            ArgType::Int => Some("value-type-int"),
            ArgType::Duration => Some("value-type-duration"),
            ArgType::Var => Some("value-type-var"),
            // Slugger strips `=` rather than hyphenating it: the heading
            // `### Value type: KEY=value` anchors as `value-type-keyvalue`.
            ArgType::KeyValue => Some("value-type-keyvalue"),
            ArgType::OneOf(_) => None,
            ArgType::Rest(inner) => inner.anchor(),
        }
    }

    /// Reference-section (title, body) for the canonical types.
    pub fn doc(&self) -> Option<(&'static str, &'static str)> {
        match self {
            ArgType::String => Some((
                "Value type: string",
                "Arbitrary text under the unified string-value rules: quotes keep exact bytes, a lone `$var` evaluates, and `{{ ... }}` placeholders interpolate.",
            )),
            ArgType::Path => Some((
                "Value type: path",
                "Workspace path, resolved against the current working directory and guarded against escaping the workspace.",
            )),
            ArgType::Int => Some((
                "Value type: int",
                "Integer, e.g. an exit code.",
            )),
            ArgType::Duration => Some((
                "Value type: duration",
                "Positive time span: a number with an `ms`, `s`, `m`, or `h` suffix — a bare number means seconds — e.g. `500ms`, `10s`, `2m`.",
            )),
            ArgType::Var => Some((
                "Value type: $var",
                "Script variable reference. The `$` sigil is mandatory.",
            )),
            ArgType::KeyValue => Some((
                "Value type: KEY=value",
                "`KEY=value` assignment splitting on the first `=` (`KEY=a=b` stores `a=b`). Values follow the unified string-value rules.",
            )),
            ArgType::OneOf(_) | ArgType::Rest(_) => None,
        }
    }
}

/// Data direction for an argument.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IoDirection {
    Read,
    Write,
}

/// Stream type for fallback or default output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stream {
    Stdin,
    Stdout,
    Stderr,
}

/// Metadata for a single flag.
pub struct FlagSpec {
    pub name: &'static str,
    pub long: &'static str,
    pub value_type: FlagValueType,
    pub required: bool,
    pub description: &'static str,
}

/// Type of value a flag accepts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlagValueType {
    /// Boolean flag (no value required).
    Flag,
    /// String-valued flag.
    String,
    /// Integer-valued flag.
    Int,
}

/// Complete metadata for a command.
pub struct CommandMeta {
    pub name: &'static str,
    pub syntax: &'static str,
    pub summary: &'static str,
    pub description: &'static str,
    pub args: &'static [ArgSpec],
    pub flags: &'static [FlagSpec],
    pub default_output: Option<Stream>,
    pub examples: &'static [Example],
}

/// An executable example for a command.
pub struct Example {
    pub name: &'static str,
    pub fence_meta: Option<&'static str>,
    pub code: &'static str,
}

/// Trait for command metadata and lowering. No execution types.
///
/// This trait lives in `oxdock-parser` and has zero dependencies on
/// `oxdock-core`. Execution dispatch is handled separately by the
/// `define_pipeline!` macro in `oxdock-core`.
pub trait CommandSpec {
    const NAME: &'static str;

    fn metadata() -> CommandMeta;
    fn lower(flags: Vec<(String, Arg)>, args: Vec<Arg>) -> Result<StepKind>;
}
