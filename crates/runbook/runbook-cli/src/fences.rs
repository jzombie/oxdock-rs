use line_ending::LineEnding;
use std::collections::HashMap;

const MAX_FENCE_PREFIX_WS: usize = 3;

#[derive(Debug)]
pub(crate) struct FenceBlock {
    pub(crate) fence: char,
    pub(crate) fence_len: usize,
    pub(crate) info: String,
    pub(crate) start_line: usize,
    pub(crate) end_line: usize,
    pub(crate) content: String,
}

pub(crate) struct FenceInfo {
    pub(crate) language: Option<String>,
    pub(crate) params: HashMap<String, String>,
}

pub(crate) fn fence_label(before: Option<&FenceBlock>, after: Option<&FenceBlock>) -> String {
    let info = before
        .and_then(|block| {
            if block.info.is_empty() {
                None
            } else {
                Some(block.info.as_str())
            }
        })
        .or_else(|| {
            after.and_then(|block| {
                if block.info.is_empty() {
                    None
                } else {
                    Some(block.info.as_str())
                }
            })
        })
        .unwrap_or("plain");
    format!("({info})")
}

pub(crate) fn display_info(info: &str) -> String {
    if info.is_empty() {
        String::from("plain")
    } else {
        info.to_string()
    }
}

pub(crate) fn parse_fences(input: &str) -> Vec<FenceBlock> {
    let mut fences = Vec::new();
    let eol = LineEnding::from_current_platform().denormalize("\n");
    let mut current: Option<FenceBlock> = None;

    for (line_idx, line) in input.lines().enumerate() {
        let line_no = line_idx + 1;
        let (ws, rest) = split_leading_ws(line);
        if let Some((fence_char, fence_len, info)) = parse_fence_start(rest) {
            match current.take() {
                Some(mut open) => {
                    if fence_char == open.fence && fence_len >= open.fence_len {
                        open.end_line = line_no;
                        fences.push(open);
                    } else {
                        open.content.push_str(line);
                        open.content.push_str(&eol);
                        current = Some(open);
                    }
                }
                None => {
                    let block = FenceBlock {
                        fence: fence_char,
                        fence_len,
                        info,
                        start_line: line_no,
                        end_line: line_no,
                        content: String::new(),
                    };
                    current = Some(block);
                }
            }
            continue;
        }

        if ws > MAX_FENCE_PREFIX_WS {
            if let Some(block) = current.as_mut() {
                block.content.push_str(line);
                block.content.push('\n');
            }
            continue;
        }

        if let Some(block) = current.as_mut() {
            block.content.push_str(line);
            block.content.push_str(&eol);
        }
    }

    if let Some(mut open) = current.take() {
        open.end_line = input.lines().count().max(open.start_line);
        fences.push(open);
    }

    fences
}

pub(crate) fn parse_fence_start(line: &str) -> Option<(char, usize, String)> {
    let trimmed = line.trim_end();
    let mut chars = trimmed.chars();
    let fence_char = chars.next()?;
    if fence_char != '`' && fence_char != '~' {
        return None;
    }

    let mut count = 1;
    for ch in chars.by_ref() {
        if ch == fence_char {
            count += 1;
        } else {
            break;
        }
    }
    if count < 3 {
        return None;
    }

    let rest = &trimmed[count..];
    let info = rest.trim().to_string();
    Some((fence_char, count, info))
}

pub(crate) fn split_leading_ws(line: &str) -> (usize, &str) {
    let mut count = 0;
    for ch in line.chars() {
        if ch == ' ' || ch == '\t' {
            count += 1;
        } else {
            break;
        }
    }
    (count, &line[count..])
}

pub(crate) fn parse_fence_info(info: &str) -> FenceInfo {
    let mut language = None;
    let mut params = HashMap::new();
    for token in info.split_whitespace() {
        if let Some((key, value)) = token.split_once('=') {
            params.insert(key.to_string(), value.trim_matches('"').to_string());
        } else if language.is_none() {
            language = Some(token.to_string());
        }
    }
    if let Some(lang) = params.get("lang") {
        language = Some(lang.clone());
    }
    FenceInfo { language, params }
}

#[cfg(test)]
mod tests {
    use super::{
        FenceBlock, display_info, fence_label, parse_fence_info, parse_fence_start, parse_fences,
        split_leading_ws,
    };
    use indoc::indoc;

    #[test]
    fn fence_helpers_parse_info_and_labels() {
        assert_eq!(display_info(""), "plain");
        assert_eq!(display_info("sh"), "sh");

        let info = parse_fence_info("sh lang=\"bash\" key=value");
        assert_eq!(info.language.as_deref(), Some("bash"));
        assert_eq!(info.params.get("key").map(String::as_str), Some("value"));

        let before = FenceBlock {
            fence: '`',
            fence_len: 3,
            info: "sh".to_string(),
            start_line: 1,
            end_line: 2,
            content: String::new(),
        };
        assert_eq!(fence_label(Some(&before), None), "(sh)");
        assert_eq!(fence_label(None, None), "(plain)");
    }

    #[test]
    fn fence_start_and_split_ws() {
        assert_eq!(split_leading_ws("   abc"), (3, "abc"));
        assert_eq!(split_leading_ws("\t\tx"), (2, "x"));

        let (ch, len, info) = parse_fence_start("```rust").expect("fence");
        assert_eq!(ch, '`');
        assert_eq!(len, 3);
        assert_eq!(info, "rust");
        assert!(parse_fence_start("``nope").is_none());
    }

    #[test]
    fn parse_fences_collects_blocks() {
        let input = indoc! {r#"
        prefix
        ```sh
        echo hi
        ```
        tail
        "#};
        let fences = parse_fences(input);
        assert_eq!(fences.len(), 1);
        assert_eq!(fences[0].info, "sh");
        assert!(fences[0].content.contains("echo hi"));
    }

    #[test]
    fn parse_fences_allows_indented_fences() {
        let input = indoc! {r#"
            1. list item
                ```bash
                echo hi
                ```
        "#};
        let fences = parse_fences(input);
        assert_eq!(fences.len(), 1);
        assert_eq!(fences[0].info, "bash");
        assert!(fences[0].content.contains("echo hi"));
    }
}
