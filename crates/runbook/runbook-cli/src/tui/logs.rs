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
