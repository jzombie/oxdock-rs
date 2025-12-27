use std::collections::VecDeque;
use std::io::{self, Write};
use std::sync::{Arc, Mutex};

use anyhow::Result;
use arboard::Clipboard;
use base64::engine::general_purpose::STANDARD as BASE64_STD;
use base64::Engine as _;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use super::config::MAX_LOG_LINES;
use super::editor::EditorState;
use super::logs::{LogRecord, LogSource, push_log_line};

pub(crate) fn run_copy_action(
    editor: &mut EditorState,
    logs: &Arc<Mutex<VecDeque<LogRecord>>>,
    clipboard: &mut Option<Clipboard>,
) -> String {
    let mut tried_clip = false;
    if clipboard.is_none() {
        match Clipboard::new() {
            Ok(new_clip) => *clipboard = Some(new_clip),
            Err(err) => {
                push_log_line(
                    logs,
                    LogSource::Stderr,
                    &format!("clipboard unavailable: {err}"),
                    MAX_LOG_LINES,
                );
            }
        }
    }
    if let Some(clip) = clipboard.as_mut() {
        tried_clip = true;
        match editor.copy_selection_to_clipboard(clip) {
            Ok(true) => {
                push_log_line(
                    logs,
                    LogSource::Stdout,
                    &format!("copy: {}", editor.selection_debug_summary()),
                    MAX_LOG_LINES,
                );
                editor.clear_selection();
                return String::from("Selection copied to clipboard");
            }
            Ok(false) => {}
            Err(err) => {
                push_log_line(
                    logs,
                    LogSource::Stderr,
                    &format!("clipboard copy failed: {err}"),
                    MAX_LOG_LINES,
                );
            }
        }
    }

    match editor.selection_text() {
        Some(text) => match osc52_copy(&text) {
            Ok(()) => {
                push_log_line(
                    logs,
                    LogSource::Stdout,
                    &format!("osc52 copy: {}", editor.selection_debug_summary()),
                    MAX_LOG_LINES,
                );
                editor.clear_selection();
                String::from("Selection copied via OSC52")
            }
            Err(err) => {
                if !tried_clip {
                    push_log_line(
                        logs,
                        LogSource::Stderr,
                        &format!("clipboard unavailable and osc52 failed: {err}"),
                        MAX_LOG_LINES,
                    );
                    String::from("Clipboard and OSC52 copy failed; see logs")
                } else {
                    push_log_line(
                        logs,
                        LogSource::Stderr,
                        &format!("OSC52 fallback failed after clipboard attempt: {err}"),
                        MAX_LOG_LINES,
                    );
                    String::from("OSC52 copy failed; see logs")
                }
            }
        },
        None => String::from("Select text to copy"),
    }
}

pub(crate) fn osc52_copy(text: &str) -> Result<()> {
    const MAX_OSC52_BYTES: usize = 100 * 1024;
    let mut payload = text.as_bytes();
    let mut truncated = false;
    if payload.len() > MAX_OSC52_BYTES {
        payload = &payload[..MAX_OSC52_BYTES];
        truncated = true;
    }
    let mut buf = Vec::with_capacity(payload.len() + 32);
    buf.extend_from_slice(payload);
    if truncated {
        buf.extend_from_slice(b"...(truncated)");
    }
    let b64 = BASE64_STD.encode(&buf);
    let seq = format!("\x1b]52;c;{b64}\x07");
    let mut out = io::stdout();
    out.write_all(seq.as_bytes())?;
    out.flush()?;
    Ok(())
}

pub(crate) fn is_copy_shortcut(event: &KeyEvent) -> bool {
    match event.code {
        KeyCode::Char('c') | KeyCode::Char('C') => event
            .modifiers
            .intersects(KeyModifiers::CONTROL | KeyModifiers::SUPER),
        KeyCode::Insert => event
            .modifiers
            .intersects(KeyModifiers::CONTROL | KeyModifiers::SHIFT),
        _ => false,
    }
}

pub(crate) fn is_paste_shortcut(event: &KeyEvent) -> bool {
    match event.code {
        KeyCode::Char('v') | KeyCode::Char('V') => event
            .modifiers
            .intersects(KeyModifiers::CONTROL | KeyModifiers::SUPER),
        KeyCode::Insert => event.modifiers == KeyModifiers::SHIFT,
        _ => false,
    }
}

pub(crate) fn log_selection_state(
    editor: &EditorState,
    logs: &Arc<Mutex<VecDeque<LogRecord>>>,
    label: &str,
) {
    let summary = editor.selection_debug_summary();
    push_log_line(
        logs,
        LogSource::Stdout,
        &format!("[SEL] {label}: {summary}"),
        MAX_LOG_LINES,
    );
}

pub(crate) fn log_key_event(
    logs: &Arc<Mutex<VecDeque<LogRecord>>>,
    label: &str,
    event: &KeyEvent,
) {
    push_log_line(
        logs,
        LogSource::Stdout,
        &format!(
            "[KEY] {label}: code={:?}, modifiers={:?}, kind={:?}",
            event.code, event.modifiers, event.kind
        ),
        MAX_LOG_LINES,
    );
}

