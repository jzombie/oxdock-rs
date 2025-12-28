use line_ending::LineEnding;
use sha2::{Digest, Sha256};

const OUTPUT_BEGIN: &str = "<!-- runbook-output:begin -->";
const OUTPUT_END: &str = "<!-- runbook-output:end -->";
const OUTPUT_META_PREFIX: &str = "<!-- runbook-output:meta";
const STDERR_MARKER: &str = "<!-- runbook-output:stderr -->";
const SHORT_HASH_LEN: usize = 32;

pub(crate) struct OutputBlock {
    pub(crate) start_index: usize,
    pub(crate) end_index: usize,
    pub(crate) code_hash: Option<String>,
    pub(crate) stdout_hash: Option<String>,
    pub(crate) stderr_hash: Option<String>,
    pub(crate) combined_hash: Option<String>,
    pub(crate) stdout: String,
    pub(crate) stderr: String,
    pub(crate) stdout_newlines: Option<usize>,
}

#[derive(Default)]
struct InlineFenceMeta {
    language: Option<String>,
    code_hash: Option<String>,
    combined_hash: Option<String>,
    newline_count: Option<usize>,
    saw_runbook: bool,
}

pub(crate) fn mark_consumed(consumed: &mut [bool], start: usize, end: usize) {
    if consumed.is_empty() {
        return;
    }
    let len = consumed.len();
    let start_idx = start.min(end).min(len);
    let end_idx = end.min(len);
    if start_idx < end_idx {
        consumed[start_idx..end_idx].fill(true);
    }
}

pub(crate) fn push_unconsumed_lines(
    consumed: &[bool],
    lines: &[&str],
    range: std::ops::Range<usize>,
    out: &mut Vec<String>,
) {
    if consumed.is_empty() || lines.is_empty() {
        return;
    }
    let len = consumed.len().min(lines.len());
    let start = range.start.min(range.end).min(len);
    let end = range.end.min(len);
    if start >= end {
        return;
    }
    for (flag, line) in consumed[start..end].iter().zip(&lines[start..end]) {
        if !*flag {
            out.push((*line).to_string());
        }
    }
}

pub(crate) fn take_detached_output_block(
    lines: &[&str],
    search_start: usize,
    code_hash: &str,
    consumed: &mut [bool],
) -> Option<OutputBlock> {
    let short = short_hash(code_hash);
    let mut idx = search_start.min(lines.len());
    while idx < lines.len() {
        if consumed.get(idx).copied().unwrap_or(false) {
            idx += 1;
            continue;
        }
        if let Some(block) = parse_output_block(lines, idx) {
            let matches = block
                .code_hash
                .as_deref()
                .is_some_and(|stored| stored == code_hash || stored == short);
            if matches {
                mark_consumed(consumed, block.start_index, block.end_index);
                return Some(block);
            }
            idx = block.end_index;
        } else {
            idx += 1;
        }
    }
    None
}

#[allow(unused_assignments)]
pub(crate) fn parse_output_block(lines: &[&str], start: usize) -> Option<OutputBlock> {
    let mut idx = start;
    while idx < lines.len() && lines[idx].trim().is_empty() {
        idx += 1;
    }
    if idx >= lines.len() {
        return None;
    }

    let mut has_begin_marker = false;
    let mut start_index = idx;
    if lines[idx].trim_end() == OUTPUT_BEGIN {
        has_begin_marker = true;
        start_index = idx;
        idx += 1;
        while idx < lines.len() && lines[idx].trim().is_empty() {
            idx += 1;
        }
    }

    let mut code_hash = None;
    let mut stdout_hash = None;
    let mut stderr_hash = None;
    let mut combined_hash = None;
    if idx < lines.len() && lines[idx].trim_start().starts_with(OUTPUT_META_PREFIX) {
        parse_output_meta(
            lines[idx].trim(),
            &mut code_hash,
            &mut stdout_hash,
            &mut stderr_hash,
            &mut combined_hash,
        );
        idx += 1;
    }
    while idx < lines.len() && lines[idx].trim().is_empty() {
        idx += 1;
    }

    if idx >= lines.len() || !lines[idx].trim_start().starts_with("```") {
        return None;
    }
    let inline_meta = parse_inline_meta(lines[idx]);
    if code_hash.is_none() {
        code_hash = inline_meta.code_hash;
    }
    if combined_hash.is_none() {
        combined_hash = inline_meta.combined_hash;
    }
    let stdout_newlines = inline_meta.newline_count;

    let is_text_output = inline_meta.language.as_deref() == Some("text");
    if !has_begin_marker
        && !is_text_output
        && code_hash.is_none()
        && combined_hash.is_none()
        && stdout_hash.is_none()
        && stderr_hash.is_none()
    {
        return None;
    }

    let (stdout, mut idx) = parse_fenced_block(lines, idx)?;
    let mut stderr = String::new();
    let mut end_index = idx;

    // Look for an optional stderr block. In the legacy format this was
    // prefixed with a comment marker; the compact format omits the marker.
    let mut peek_idx = idx;
    while peek_idx < lines.len() && lines[peek_idx].trim().is_empty() {
        peek_idx += 1;
    }

    if peek_idx < lines.len() && lines[peek_idx].trim_start() == STDERR_MARKER {
        let mut stderr_start = peek_idx + 1;
        while stderr_start < lines.len() && lines[stderr_start].trim().is_empty() {
            stderr_start += 1;
        }
        if stderr_start < lines.len()
            && let Some((parsed_stderr, next_idx)) = parse_fenced_block(lines, stderr_start)
        {
            let stderr_meta = parse_inline_meta(lines[stderr_start]);
            if stderr_meta.language.as_deref() == Some("text") {
                stderr = parsed_stderr;
                end_index = next_idx;
                idx = next_idx;
            }
        }
    } else if peek_idx < lines.len()
        && lines[peek_idx].trim_start().starts_with("```")
        && let Some((parsed_stderr, next_idx)) = parse_fenced_block(lines, peek_idx)
    {
        let stderr_meta = parse_inline_meta(lines[peek_idx]);
        if stderr_meta.language.as_deref() == Some("text") && stderr_meta.saw_runbook {
            stderr = parsed_stderr;
            end_index = next_idx;
            idx = next_idx;
        }
    }

    if has_begin_marker {
        let mut end_seek = idx;
        while end_seek < lines.len() && lines[end_seek].trim().is_empty() {
            end_seek += 1;
        }
        if end_seek >= lines.len() || lines[end_seek].trim_end() != OUTPUT_END {
            return None;
        }
        end_index = end_seek + 1;
    }

    Some(OutputBlock {
        start_index,
        end_index,
        code_hash,
        stdout_hash,
        stderr_hash,
        combined_hash,
        stdout,
        stderr,
        stdout_newlines,
    })
}

pub(crate) fn parse_fenced_block(lines: &[&str], fence_idx: usize) -> Option<(String, usize)> {
    if fence_idx >= lines.len() || !lines[fence_idx].trim_start().starts_with("```") {
        return None;
    }

    let mut idx = fence_idx + 1;
    let mut body: Vec<&str> = Vec::new();
    while idx < lines.len() {
        if lines[idx].trim_end() == "```" {
            return Some((body.join("\n"), idx + 1));
        }
        body.push(lines[idx]);
        idx += 1;
    }
    None
}

pub(crate) fn parse_output_meta(
    line: &str,
    code_hash: &mut Option<String>,
    stdout_hash: &mut Option<String>,
    stderr_hash: &mut Option<String>,
    combined_hash: &mut Option<String>,
) {
    let trimmed = line
        .trim_start_matches("<!--")
        .trim_end_matches("-->")
        .trim();
    let tokens: Vec<&str> = trimmed.split_whitespace().collect();
    for token in tokens {
        if let Some(value) = token.strip_prefix("code=") {
            *code_hash = Some(value.to_string());
        } else if let Some(value) = token.strip_prefix("stdout=") {
            *stdout_hash = Some(value.to_string());
        } else if let Some(value) = token.strip_prefix("stderr=") {
            *stderr_hash = Some(value.to_string());
        } else if let Some(value) = token.strip_prefix("hash=") {
            *combined_hash = Some(value.to_string());
        } else if let Some(value) = token.strip_prefix("output=") {
            // Backwards-compatible: single output hash stored as stdout and combined.
            *stdout_hash = Some(value.to_string());
            combined_hash.get_or_insert_with(|| value.to_string());
        }
    }
}

fn parse_inline_meta(line: &str) -> InlineFenceMeta {
    let trimmed = line.trim_start();
    if !trimmed.starts_with("```") {
        return InlineFenceMeta::default();
    }
    let mut tokens = trimmed.trim_start_matches('`').split_whitespace();
    let mut meta = InlineFenceMeta {
        language: tokens.next().map(|tok| tok.to_string()),
        ..InlineFenceMeta::default()
    };
    for token in tokens {
        if token == "runbook" {
            meta.saw_runbook = true;
            continue;
        }
        if let Some(value) = token.strip_prefix("code=") {
            meta.code_hash = Some(value.to_string());
        } else if let Some(value) = token.strip_prefix("hash=") {
            meta.combined_hash = Some(value.to_string());
        } else if let Some(value) = token.strip_prefix("nl=")
            && let Ok(parsed) = value.parse::<usize>()
        {
            meta.newline_count = Some(parsed);
        }
    }
    meta
}

pub(crate) fn format_output_block(code_hash: &str, stdout: &str, stderr: &str) -> Vec<String> {
    let mut lines = Vec::new();
    let stdout_newlines = count_trailing_newlines(stdout);
    let stderr_newlines = count_trailing_newlines(stderr);
    let stdout_norm = normalize_output(stdout);
    let stderr_norm = normalize_output(stderr);
    let combined_hash = combined_output_hash(&stdout_norm, &stderr_norm);
    let code_hash_short = short_hash(code_hash);
    let combined_hash_short = short_hash(&combined_hash);
    // First fenced block: stdout (may be empty). Keep it minimal for humans.
    lines.push(format!(
        "```text runbook code={code_hash_short} hash={combined_hash_short} nl={stdout_newlines}"
    ));
    if !stdout_norm.is_empty() {
        for line in stdout_norm.lines() {
            lines.push(line.to_string());
        }
    }
    lines.push("```".to_string());

    // If stderr is present, emit a second fenced block that is visibly marked
    // as an error for humans reading the Markdown in a terminal.
    if !stderr_norm.is_empty() {
        lines.push(String::new());
        lines.push(format!(
            "```text runbook code={code_hash_short} hash={combined_hash_short} nl={stderr_newlines}"
        ));
        let mut s_lines = stderr_norm.lines();
        if let Some(first) = s_lines.next() {
            lines.push(format!("ERROR: {first}"));
            for line in s_lines {
                lines.push(line.to_string());
            }
        }
        lines.push("```".to_string());
    }
    lines
}

pub(crate) fn combined_output_hash(stdout_norm: &str, stderr_norm: &str) -> String {
    // Hash stdout and stderr together so we can track changes with a single token.
    let mut hasher = Sha256::new();
    hasher.update(stdout_norm.as_bytes());
    hasher.update(b"\n--stderr--\n");
    hasher.update(stderr_norm.as_bytes());
    let digest = hasher.finalize();
    format!("{digest:x}")
}

pub(crate) fn sha256_hex(input: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(input.as_bytes());
    let digest = hasher.finalize();
    format!("{digest:x}")
}

pub(crate) fn short_hash(full_hex: &str) -> String {
    full_hex.chars().take(SHORT_HASH_LEN).collect()
}

pub(crate) fn normalize_output(output: &str) -> String {
    let norm = LineEnding::normalize(output);
    norm.trim_end_matches('\n').to_string()
}

pub(crate) fn count_trailing_newlines(output: &str) -> usize {
    let norm = LineEnding::normalize(output);
    norm.chars().rev().take_while(|ch| *ch == '\n').count()
}

pub(crate) fn restore_with_trailing_newlines(value: &str, newline_count: Option<usize>) -> String {
    let extra = newline_count.unwrap_or(if value.is_empty() { 0 } else { 1 });
    if extra == 0 {
        return value.to_string();
    }
    let eol = LineEnding::from_current_platform().denormalize("\n");
    let mut restored = String::with_capacity(value.len() + extra * eol.len());
    restored.push_str(value);
    for _ in 0..extra {
        restored.push_str(&eol);
    }
    restored
}

#[cfg(test)]
mod tests {
    use super::{
        OUTPUT_BEGIN, OUTPUT_END, STDERR_MARKER, combined_output_hash, count_trailing_newlines,
        format_output_block, mark_consumed, normalize_output, parse_fenced_block,
        parse_inline_meta, parse_output_block, parse_output_meta, push_unconsumed_lines,
        restore_with_trailing_newlines, short_hash, take_detached_output_block,
    };
    use line_ending::LineEnding;

    #[test]
    fn parse_inline_meta_and_output_meta() {
        let meta = parse_inline_meta("```text runbook code=abc hash=def nl=2");
        assert_eq!(meta.language.as_deref(), Some("text"));
        assert_eq!(meta.code_hash.as_deref(), Some("abc"));
        assert_eq!(meta.combined_hash.as_deref(), Some("def"));
        assert_eq!(meta.newline_count, Some(2));
        assert!(meta.saw_runbook);

        let mut code = None;
        let mut stdout = None;
        let mut stderr = None;
        let mut combined = None;
        parse_output_meta(
            "<!-- runbook-output:meta code=aa stdout=bb stderr=cc hash=dd -->",
            &mut code,
            &mut stdout,
            &mut stderr,
            &mut combined,
        );
        assert_eq!(code.as_deref(), Some("aa"));
        assert_eq!(stdout.as_deref(), Some("bb"));
        assert_eq!(stderr.as_deref(), Some("cc"));
        assert_eq!(combined.as_deref(), Some("dd"));
    }

    #[test]
    fn output_block_parsing_and_formatting() {
        let lines = vec![
            OUTPUT_BEGIN,
            "<!-- runbook-output:meta code=abc hash=def -->",
            "```text runbook code=abc hash=def nl=1",
            "hello",
            "```",
            OUTPUT_END,
        ];
        let refs: Vec<&str> = lines.iter().copied().collect();
        let block = parse_output_block(&refs, 0).expect("block");
        assert_eq!(block.stdout, "hello");
        assert_eq!(block.code_hash.as_deref(), Some("abc"));
        assert_eq!(block.combined_hash.as_deref(), Some("def"));
        assert_eq!(block.stdout_newlines, Some(1));

        let rendered = format_output_block("abc", "out\n", "err\n");
        assert!(rendered.iter().any(|line| line.contains("runbook")));
        assert!(rendered.iter().any(|line| line.contains("ERROR: err")));
    }

    #[test]
    fn output_normalization_and_restore() {
        let normalized = normalize_output("hi\r\n");
        assert!(!normalized.contains('\r'));
        assert!(normalized.starts_with("hi"));
        assert_eq!(count_trailing_newlines("hi\n\n"), 2);
        assert_eq!(short_hash("abcdef"), "abcdef");

        let restored = restore_with_trailing_newlines("ok", Some(2));
        let eol = LineEnding::from_current_platform().denormalize("\n");
        assert_eq!(restored, format!("ok{eol}{eol}"));
    }

    #[test]
    fn consumed_line_helpers_track_ranges() {
        let mut consumed = vec![false; 5];
        mark_consumed(&mut consumed, 1, 4);
        assert_eq!(consumed, vec![false, true, true, true, false]);

        let lines = vec!["a", "b", "c", "d", "e"];
        let mut out = Vec::new();
        push_unconsumed_lines(&consumed, &lines, 0..5, &mut out);
        assert_eq!(out, vec!["a".to_string(), "e".to_string()]);
    }

    #[test]
    fn parse_fenced_blocks_with_stderr() {
        let lines = vec![
            "```text runbook code=abc hash=def nl=1",
            "out",
            "```",
            "",
            STDERR_MARKER,
            "```text runbook code=abc hash=def nl=1",
            "err",
            "```",
        ];
        let refs: Vec<&str> = lines.iter().copied().collect();
        let block = parse_output_block(&refs, 0).expect("block");
        assert_eq!(block.stdout, "out");
        assert_eq!(block.stderr, "err");
    }

    #[test]
    fn take_detached_output_block_matches_hash() {
        let code_hash = "abc";
        let block_lines = vec![
            OUTPUT_BEGIN,
            "<!-- runbook-output:meta code=abc hash=def -->",
            "```text runbook code=abc hash=def nl=1",
            "out",
            "```",
            OUTPUT_END,
        ];
        let refs: Vec<&str> = block_lines.iter().copied().collect();
        let mut consumed = vec![false; refs.len()];
        let block = take_detached_output_block(&refs, 0, code_hash, &mut consumed).expect("block");
        assert_eq!(block.stdout, "out");
        assert!(consumed.iter().any(|flag| *flag));
    }

    #[test]
    fn parse_fenced_block_requires_closing() {
        let lines = vec!["```text", "out"];
        let refs: Vec<&str> = lines.iter().copied().collect();
        assert!(parse_fenced_block(&refs, 0).is_none());
    }

    #[test]
    fn format_output_block_without_stderr() {
        let lines = format_output_block("abc", "only\n", "");
        assert!(lines.iter().any(|line| line.contains("```text")));
        assert!(!lines.iter().any(|line| line.contains("ERROR:")));
    }

    #[test]
    fn combined_hash_is_sha256_len() {
        let hash = combined_output_hash("a", "b");
        assert_eq!(hash.len(), 64);
    }
}
