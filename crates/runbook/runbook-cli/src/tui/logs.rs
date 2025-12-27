use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

use ratatui::Frame;
use ratatui::backend::Backend;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Span, Spans, Text};
use ratatui::widgets::{Block, Borders, Paragraph};

use super::views::FramedView;

#[derive(Clone)]
pub(crate) struct LogRecord {
    pub source: LogSource,
    pub content: String,
}

impl LogRecord {
    pub(crate) fn new(source: LogSource, content: String) -> Self {
        Self { source, content }
    }
}

#[derive(Clone, Copy)]
pub(crate) enum LogSource {
    Stdout,
    Stderr,
}

impl LogSource {
    pub(crate) fn label(self) -> &'static str {
        match self {
            LogSource::Stdout => "[out]",
            LogSource::Stderr => "[err]",
        }
    }

    pub(crate) fn style(self) -> Style {
        match self {
            LogSource::Stdout => Style::default().fg(Color::LightGreen),
            LogSource::Stderr => Style::default().fg(Color::LightRed),
        }
    }
}

pub(crate) struct LogsView<'a> {
    logs: &'a Arc<Mutex<VecDeque<LogRecord>>>,
    scroll: &'a mut LogScrollState,
}

impl<'a> LogsView<'a> {
    pub(crate) fn new(
        logs: &'a Arc<Mutex<VecDeque<LogRecord>>>,
        scroll: &'a mut LogScrollState,
    ) -> Self {
        Self { logs, scroll }
    }
}

impl<'a> FramedView for LogsView<'a> {
    fn block<'b>(&'b self) -> Block<'b> {
        Block::default().borders(Borders::ALL).title("Logs")
    }

    fn render_inner<B: Backend>(&mut self, frame: &mut Frame<'_, B>, inner: Rect) {
        let text = {
            let guard = self.logs.lock().unwrap();
            let viewport = inner.height as usize;
            self.scroll.update_metrics(guard.len(), viewport.max(1));
            if guard.is_empty() {
                Text::from("(no logs yet)")
            } else {
                let start = self.scroll.offset();
                logs_to_text_window(&guard, start, viewport.max(1))
            }
        };
        frame.render_widget(Paragraph::new(text), inner);

        if let Some(label) = self.scroll.indicator_text()
            && inner.width > 0
        {
            let width = inner.width.min(16);
            let x = inner.x + inner.width - width;
            let rect = Rect {
                x,
                y: inner.y,
                width,
                height: 1,
            };
            let indicator = Paragraph::new(label)
                .alignment(ratatui::layout::Alignment::Right)
                .style(Style::default().fg(Color::DarkGray));
            frame.render_widget(indicator, rect);
        }
    }
}

pub(crate) struct LogScrollState {
    offset: usize,
    viewport: usize,
    total_lines: usize,
    follow_tail: bool,
}

impl Default for LogScrollState {
    fn default() -> Self {
        Self {
            offset: 0,
            viewport: 0,
            total_lines: 0,
            follow_tail: true,
        }
    }
}

impl LogScrollState {
    pub(crate) fn offset(&self) -> usize {
        self.offset
    }

    pub(crate) fn update_metrics(&mut self, total_lines: usize, viewport: usize) {
        self.total_lines = total_lines;
        self.viewport = viewport.max(1);
        if self.follow_tail {
            self.offset = self.max_offset();
        } else {
            self.clamp_to_bounds();
        }
    }

    pub(crate) fn scroll(&mut self, delta: isize) {
        if delta == 0 {
            return;
        }
        let max_offset = self.max_offset();
        if delta > 0 {
            let delta = delta as usize;
            self.offset = (self.offset + delta).min(max_offset);
            self.follow_tail = self.offset == max_offset;
        } else {
            let delta = (-delta) as usize;
            self.offset = self.offset.saturating_sub(delta);
            self.follow_tail = false;
        }
    }

    pub(crate) fn indicator_text(&self) -> Option<String> {
        if self.total_lines == 0 || self.viewport == 0 {
            return None;
        }
        let start = self.offset + 1;
        let end = (self.offset + self.viewport).min(self.total_lines);
        Some(format!("{start}-{end}/{total}", total = self.total_lines))
    }

    fn clamp_to_bounds(&mut self) {
        let max_offset = self.max_offset();
        if self.offset > max_offset {
            self.offset = max_offset;
        }
        if self.offset < max_offset {
            self.follow_tail = false;
        } else if self.total_lines <= self.viewport {
            self.follow_tail = true;
        }
    }

    fn max_offset(&self) -> usize {
        self.total_lines.saturating_sub(self.viewport)
    }
}

pub(crate) fn push_log_line(
    logs: &Arc<Mutex<VecDeque<LogRecord>>>,
    source: LogSource,
    line: &str,
    max_logs: usize,
) {
    let mut guard = logs.lock().unwrap();
    guard.push_back(LogRecord::new(source, format!("{line}\n")));
    trim_logs(&mut guard, max_logs);
}

pub(crate) fn trim_logs(logs: &mut VecDeque<LogRecord>, max: usize) {
    while logs.len() > max {
        logs.pop_front();
    }
}

pub(crate) fn logs_to_text_window(
    entries: &VecDeque<LogRecord>,
    start: usize,
    max_lines: usize,
) -> Text<'static> {
    if max_lines == 0 {
        return Text::from("");
    }
    let clamped_start = start.min(entries.len());
    let end = (clamped_start + max_lines).min(entries.len());
    let mut lines = Vec::new();
    for record in entries
        .iter()
        .skip(clamped_start)
        .take(end.saturating_sub(clamped_start))
    {
        let mut spans = Vec::new();
        spans.push(Span::styled(record.source.label(), record.source.style()));
        spans.push(Span::raw(" "));
        let content = record.content.trim_end_matches(['\n', '\r']);
        spans.extend(parse_ansi_spans(content));
        lines.push(Spans::from(spans));
    }
    if lines.is_empty() {
        lines.push(Spans::from(""));
    }
    Text::from(lines)
}

fn parse_ansi_spans(input: &str) -> Vec<Span<'static>> {
    let mut spans = Vec::new();
    let mut buffer = String::new();
    let mut style = Style::default();
    let mut chars = input.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\u{1b}' {
            if let Some('[') = chars.peek() {
                chars.next();
                flush_span(&mut buffer, &mut spans, style);
                let mut seq = String::new();
                for next in chars.by_ref() {
                    if next.is_ascii_alphabetic() {
                        if next == 'm' {
                            apply_sgr_sequence(&seq, &mut style);
                        }
                        break;
                    } else {
                        seq.push(next);
                    }
                }
            }
            continue;
        }
        if ch == '\r' {
            continue;
        }
        buffer.push(ch);
    }

    flush_span(&mut buffer, &mut spans, style);
    if spans.is_empty() {
        spans.push(Span::raw(""));
    }
    spans
}

fn flush_span(buffer: &mut String, spans: &mut Vec<Span<'static>>, style: Style) {
    if buffer.is_empty() {
        return;
    }
    let text = std::mem::take(buffer);
    spans.push(Span::styled(text, style));
}

fn apply_sgr_sequence(sequence: &str, style: &mut Style) {
    let mut numbers: Vec<i32> = sequence
        .split(';')
        .map(|part| {
            if part.is_empty() {
                0
            } else {
                part.parse().unwrap_or(0)
            }
        })
        .collect();
    if numbers.is_empty() {
        numbers.push(0);
    }

    let mut iter = numbers.into_iter();
    while let Some(code) = iter.next() {
        match code {
            0 => *style = Style::default(),
            1 => *style = style.add_modifier(Modifier::BOLD),
            2 => *style = style.add_modifier(Modifier::DIM),
            3 => *style = style.add_modifier(Modifier::ITALIC),
            4 => *style = style.add_modifier(Modifier::UNDERLINED),
            5 => *style = style.add_modifier(Modifier::SLOW_BLINK),
            6 => *style = style.add_modifier(Modifier::RAPID_BLINK),
            7 => *style = style.add_modifier(Modifier::REVERSED),
            8 => *style = style.add_modifier(Modifier::HIDDEN),
            9 => *style = style.add_modifier(Modifier::CROSSED_OUT),
            22 => *style = style.remove_modifier(Modifier::BOLD | Modifier::DIM),
            23 => *style = style.remove_modifier(Modifier::ITALIC),
            24 => *style = style.remove_modifier(Modifier::UNDERLINED),
            25 => *style = style.remove_modifier(Modifier::SLOW_BLINK | Modifier::RAPID_BLINK),
            27 => *style = style.remove_modifier(Modifier::REVERSED),
            28 => *style = style.remove_modifier(Modifier::HIDDEN),
            29 => *style = style.remove_modifier(Modifier::CROSSED_OUT),
            30..=37 => {
                if let Some(color) = ansi_basic_color((code - 30) as u8, false) {
                    *style = style.fg(color);
                }
            }
            90..=97 => {
                if let Some(color) = ansi_basic_color((code - 90) as u8, true) {
                    *style = style.fg(color);
                }
            }
            39 => *style = style.fg(Color::Reset),
            40..=47 => {
                if let Some(color) = ansi_basic_color((code - 40) as u8, false) {
                    *style = style.bg(color);
                }
            }
            100..=107 => {
                if let Some(color) = ansi_basic_color((code - 100) as u8, true) {
                    *style = style.bg(color);
                }
            }
            49 => *style = style.bg(Color::Reset),
            38 | 48 => {
                let is_fg = code == 38;
                if let Some(mode) = iter.next() {
                    match mode {
                        5 => {
                            if let Some(idx) = iter.next() {
                                let color = Color::Indexed(idx as u8);
                                *style = if is_fg {
                                    style.fg(color)
                                } else {
                                    style.bg(color)
                                };
                            }
                        }
                        2 => {
                            let r = iter.next().unwrap_or(0) as u8;
                            let g = iter.next().unwrap_or(0) as u8;
                            let b = iter.next().unwrap_or(0) as u8;
                            let color = Color::Rgb(r, g, b);
                            *style = if is_fg {
                                style.fg(color)
                            } else {
                                style.bg(color)
                            };
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
}

fn ansi_basic_color(code: u8, bright: bool) -> Option<Color> {
    let color = match code {
        0 => {
            if bright {
                Color::DarkGray
            } else {
                Color::Black
            }
        }
        1 => {
            if bright {
                Color::LightRed
            } else {
                Color::Red
            }
        }
        2 => {
            if bright {
                Color::LightGreen
            } else {
                Color::Green
            }
        }
        3 => {
            if bright {
                Color::LightYellow
            } else {
                Color::Yellow
            }
        }
        4 => {
            if bright {
                Color::LightBlue
            } else {
                Color::Blue
            }
        }
        5 => {
            if bright {
                Color::LightMagenta
            } else {
                Color::Magenta
            }
        }
        6 => {
            if bright {
                Color::LightCyan
            } else {
                Color::Cyan
            }
        }
        7 => {
            if bright {
                Color::White
            } else {
                Color::Gray
            }
        }
        _ => return None,
    };
    Some(color)
}

#[cfg(test)]
mod tests {
    use super::{
        LogRecord, LogScrollState, LogSource, ansi_basic_color, apply_sgr_sequence,
        logs_to_text_window, parse_ansi_spans, push_log_line, trim_logs,
    };
    use ratatui::style::{Color, Modifier, Style};
    use std::collections::VecDeque;
    use std::sync::{Arc, Mutex};

    #[test]
    fn log_source_label_and_style() {
        let out = LogSource::Stdout;
        let err = LogSource::Stderr;
        assert_eq!(out.label(), "[out]");
        assert_eq!(err.label(), "[err]");
        assert_eq!(out.style().fg, Some(Color::LightGreen));
        assert_eq!(err.style().fg, Some(Color::LightRed));
    }

    #[test]
    fn scroll_state_tracks_offsets_and_indicator() {
        let mut state = LogScrollState::default();
        state.update_metrics(10, 4);
        assert_eq!(state.offset(), 6);
        assert_eq!(state.indicator_text().as_deref(), Some("7-10/10"));

        state.scroll(-2);
        assert_eq!(state.offset(), 4);
        assert_eq!(state.indicator_text().as_deref(), Some("5-8/10"));

        state.scroll(10);
        assert_eq!(state.offset(), 6);
        assert_eq!(state.indicator_text().as_deref(), Some("7-10/10"));
    }

    #[test]
    fn push_log_line_trims() {
        let logs = Arc::new(Mutex::new(VecDeque::new()));
        push_log_line(&logs, LogSource::Stdout, "one", 2);
        push_log_line(&logs, LogSource::Stdout, "two", 2);
        push_log_line(&logs, LogSource::Stdout, "three", 2);

        let guard = logs.lock().unwrap();
        assert_eq!(guard.len(), 2);
        assert_eq!(guard[0].content, "two\n");
        assert_eq!(guard[1].content, "three\n");
    }

    #[test]
    fn trim_logs_removes_oldest() {
        let mut logs = VecDeque::new();
        logs.push_back(LogRecord::new(LogSource::Stdout, "a".to_string()));
        logs.push_back(LogRecord::new(LogSource::Stdout, "b".to_string()));
        logs.push_back(LogRecord::new(LogSource::Stdout, "c".to_string()));
        trim_logs(&mut logs, 2);
        assert_eq!(logs.len(), 2);
        assert_eq!(logs[0].content, "b");
        assert_eq!(logs[1].content, "c");
    }

    #[test]
    fn logs_to_text_window_parses_ansi() {
        let mut entries = VecDeque::new();
        entries.push_back(LogRecord::new(
            LogSource::Stdout,
            "hello \u{1b}[31mred\u{1b}[0m".to_string(),
        ));
        let text = logs_to_text_window(&entries, 0, 1);
        let spans = &text.lines[0].0;
        assert_eq!(spans[0].content.as_ref(), "[out]");
        assert_eq!(spans[1].content.as_ref(), " ");
        assert_eq!(spans[2].content.as_ref(), "hello ");
        assert_eq!(spans[2].style, Style::default());
        assert_eq!(spans[3].content.as_ref(), "red");
        assert_eq!(spans[3].style.fg, Some(Color::Red));
    }

    #[test]
    fn parse_ansi_spans_resets_style() {
        let spans = parse_ansi_spans("\u{1b}[1;32mgo\u{1b}[0m idle");
        assert_eq!(spans[0].content.as_ref(), "go");
        assert!(spans[0].style.add_modifier.contains(Modifier::BOLD));
        assert_eq!(spans[0].style.fg, Some(Color::Green));
        assert_eq!(spans[1].content.as_ref(), " idle");
        assert_eq!(spans[1].style, Style::default());
    }

    #[test]
    fn apply_sgr_sequence_handles_colors_and_modifiers() {
        let mut style = Style::default();
        apply_sgr_sequence("1;31", &mut style);
        assert!(style.add_modifier.contains(Modifier::BOLD));
        assert_eq!(style.fg, Some(Color::Red));

        apply_sgr_sequence("22;39", &mut style);
        assert!(!style.add_modifier.contains(Modifier::BOLD));
        assert_eq!(style.fg, Some(Color::Reset));
    }

    #[test]
    fn ansi_basic_color_handles_bright() {
        assert_eq!(ansi_basic_color(2, false), Some(Color::Green));
        assert_eq!(ansi_basic_color(2, true), Some(Color::LightGreen));
        assert_eq!(ansi_basic_color(9, true), None);
    }
}
