use anyhow::{Context, Result};
use crossterm::{
    event::{self, Event as CEvent, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Alignment, Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Span, Spans, Text},
    widgets::{Block, Borders, Paragraph},
    Terminal,
};
use std::collections::VecDeque;
use std::fs;
use std::io::{self, BufReader, Read};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, SystemTime};

fn main() -> Result<()> {
    let mut use_tui = false;
    let mut cli_args = Vec::new();
    for arg in std::env::args().skip(1) {
        match arg.as_str() {
            "--tui" | "-t" => use_tui = true,
            "tui" if !use_tui && cli_args.is_empty() => use_tui = true,
            _ => cli_args.push(arg),
        }
    }

    if use_tui {
        return run_tui(cli_args);
    }

    oxbook_cli::run()
}

fn run_tui(cli_args: Vec<String>) -> Result<()> {
    const MAX_LOG_LINES: usize = 800;
    let target_path = resolve_target(&cli_args);

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.clear()?;

    let result = (|| -> Result<()> {
        let logs: Arc<Mutex<VecDeque<LogRecord>>> = Arc::new(Mutex::new(VecDeque::new()));
        let mut status = String::from("Idle");
        let mut child: Option<Child> = None;
        let mut mode = UiMode::Dashboard;

        loop {
            if let Some(proc) = child.as_mut() {
                if let Some(exit) = proc.try_wait()? {
                    let mut guard = logs.lock().unwrap();
                    guard.push_back(LogRecord::new(
                        LogSource::Stdout,
                        format!("process exited: {}\n", exit),
                    ));
                    trim_logs(&mut guard, MAX_LOG_LINES);
                    child = None;
                    status = String::from("Idle");
                }
            }

            if let UiMode::Editor(editor) = &mut mode {
                if let Some(message) = editor.sync_with_disk()? {
                    status = message;
                }
            }

            terminal.draw(|f| {
                let size = f.size();
                let layout = Layout::default()
                    .direction(Direction::Vertical)
                    .margin(2)
                    .constraints([
                        Constraint::Length(3),
                        Constraint::Min(1),
                        Constraint::Length(3),
                    ])
                    .split(size);

                let header_text = match mode {
                    UiMode::Dashboard => "Oxbook TUI — 'r' run • 'e' edit • 'q' quit",
                    UiMode::Editor(_) => "Editor — Esc close • Ctrl-S save • arrows move",
                };
                let header = Paragraph::new(header_text)
                    .block(Block::default().borders(Borders::ALL).title("oxbook-cli"))
                    .alignment(Alignment::Center);
                f.render_widget(header, layout[0]);

                let mid = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([Constraint::Min(3), Constraint::Length(12)])
                    .split(layout[1]);

                match &mut mode {
                    UiMode::Dashboard => {
                        let target_info = if cli_args.is_empty() {
                            "No path provided (defaults to README.md)".to_string()
                        } else {
                            format!("Target: {}", cli_args.join(" "))
                        };
                        let body = Paragraph::new(format!(
                            "Watch + Logs\n\n{target_info}\nPress 'e' to open the inline editor, 'r' to run oxbook-cli within this session."
                        ))
                        .block(Block::default().borders(Borders::ALL).title("Controls"));
                        f.render_widget(body, mid[0]);
                    }
                    UiMode::Editor(editor) => {
                        let inner_height = mid[0].height.saturating_sub(2) as usize;
                        let viewport = inner_height.max(1);
                        editor.adjust_scroll(viewport);
                        let text = editor.visible_text(viewport);
                        let title = format!("Editing {}", editor.short_path());
                        let editor_widget = Paragraph::new(text)
                            .block(Block::default().borders(Borders::ALL).title(title));
                        f.render_widget(editor_widget, mid[0]);

                        let inner_width = mid[0].width.saturating_sub(2) as usize;
                        let cursor_x = mid[0].x + 1 + editor.cursor_col().min(inner_width) as u16;
                        let cursor_row = editor.relative_cursor_row().min(viewport - 1);
                        let cursor_y = mid[0].y + 1 + cursor_row as u16;
                        f.set_cursor(cursor_x, cursor_y);
                    }
                }

                let log_text = {
                    let guard = logs.lock().unwrap();
                    if guard.is_empty() {
                        Text::from("(no logs yet)")
                    } else {
                        let inner_height = mid[1].height.saturating_sub(2) as usize;
                        logs_to_text(&guard, inner_height.max(1))
                    }
                };
                let logs_widget = Paragraph::new(log_text)
                    .block(Block::default().borders(Borders::ALL).title("Logs"));
                f.render_widget(logs_widget, mid[1]);

                let footer = Paragraph::new(status.clone())
                    .block(Block::default().borders(Borders::ALL).title("Status"))
                    .style(Style::default().add_modifier(Modifier::BOLD));
                f.render_widget(footer, layout[2]);
            })?;

            if event::poll(Duration::from_millis(120))? {
                if let CEvent::Key(key_event) = event::read()? {
                    if key_event.kind != KeyEventKind::Press {
                        continue;
                    }
                    match &mut mode {
                        UiMode::Dashboard => match key_event.code {
                            KeyCode::Char('q') => {
                                if let Some(mut proc) = child.take() {
                                    let _ = proc.kill();
                                    let _ = proc.wait();
                                }
                                break Ok(());
                            }
                            KeyCode::Char('r') => {
                                if child.is_some() {
                                    status = String::from("Already running");
                                    continue;
                                }
                                match spawn_cli_child(&cli_args, logs.clone(), MAX_LOG_LINES) {
                                    Ok(new_child) => {
                                        status = String::from("Running...");
                                        child = Some(new_child);
                                    }
                                    Err(err) => {
                                        let mut guard = logs.lock().unwrap();
                                        guard.push_back(LogRecord::new(
                                            LogSource::Stderr,
                                            format!("spawn error: {err}\n"),
                                        ));
                                        trim_logs(&mut guard, MAX_LOG_LINES);
                                        status = String::from("Spawn failed");
                                    }
                                }
                            }
                            KeyCode::Char('e') => match EditorState::load(target_path.clone()) {
                                Ok(editor) => {
                                    status = format!("Editing {}", editor.short_path());
                                    mode = UiMode::Editor(editor);
                                }
                                Err(err) => {
                                    let mut guard = logs.lock().unwrap();
                                    guard.push_back(LogRecord::new(
                                        LogSource::Stderr,
                                        format!("editor load error: {err}\n"),
                                    ));
                                    trim_logs(&mut guard, MAX_LOG_LINES);
                                    status = String::from("Editor failed");
                                }
                            },
                            _ => {}
                        },
                        UiMode::Editor(editor) => {
                            let action = editor.handle_key(key_event)?;
                            let message = editor.take_status_message();
                            if matches!(action, EditorAction::Exit) {
                                let closed = editor.short_path();
                                status = if let Some(msg) = message {
                                    format!("{msg} • Editor closed ({closed})")
                                } else {
                                    format!("Editor closed ({closed})")
                                };
                                mode = UiMode::Dashboard;
                            } else if let Some(msg) = message {
                                status = msg;
                            }
                        }
                    }
                }
            }
        }
    })();

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;
    result
}

fn resolve_target(cli_args: &[String]) -> PathBuf {
    if let Some(first) = cli_args.first() {
        PathBuf::from(first)
    } else {
        PathBuf::from("README.md")
    }
}

enum UiMode {
    Dashboard,
    Editor(EditorState),
}

enum EditorAction {
    Continue,
    Exit,
}

struct EditorState {
    path: PathBuf,
    lines: Vec<String>,
    cursor_row: usize,
    cursor_col: usize,
    scroll_row: usize,
    dirty: bool,
    last_disk_mtime: Option<SystemTime>,
    pending_status: Option<String>,
}

impl EditorState {
    fn load(path: PathBuf) -> Result<Self> {
        let (lines, last_disk_mtime) = Self::read_disk_snapshot(&path)?;
        Ok(Self {
            path,
            lines,
            cursor_row: 0,
            cursor_col: 0,
            scroll_row: 0,
            dirty: false,
            last_disk_mtime,
            pending_status: None,
        })
    }

    fn read_disk_snapshot(path: &Path) -> Result<(Vec<String>, Option<SystemTime>)> {
        let contents = match fs::read_to_string(path) {
            Ok(buf) => buf,
            Err(err) if err.kind() == io::ErrorKind::NotFound => String::new(),
            Err(err) => return Err(err.into()),
        };
        let lines = Self::split_into_lines(contents);
        let modified = Self::disk_mtime(path);
        Ok((lines, modified))
    }

    fn split_into_lines(contents: String) -> Vec<String> {
        let mut lines: Vec<String> = if contents.is_empty() {
            vec![String::new()]
        } else {
            contents.lines().map(|line| line.to_string()).collect()
        };
        if lines.is_empty() {
            lines.push(String::new());
        }
        lines
    }

    fn disk_mtime(path: &Path) -> Option<SystemTime> {
        fs::metadata(path).ok().and_then(|meta| meta.modified().ok())
    }

    fn sync_with_disk(&mut self) -> Result<Option<String>> {
        if self.dirty {
            return Ok(None);
        }

        let modified = Self::disk_mtime(&self.path);
        if modified == self.last_disk_mtime {
            return Ok(None);
        }

        let contents = match fs::read_to_string(&self.path) {
            Ok(buf) => buf,
            Err(err) if err.kind() == io::ErrorKind::NotFound => String::new(),
            Err(err) => return Err(err.into()),
        };
        let new_lines = Self::split_into_lines(contents);
        if new_lines == self.lines {
            self.last_disk_mtime = modified;
            return Ok(None);
        }

        self.lines = new_lines;
        self.clamp_cursor();
        self.last_disk_mtime = modified;
        Ok(Some(format!("Reloaded {}", self.short_path())))
    }

    fn clamp_cursor(&mut self) {
        if self.lines.is_empty() {
            self.lines.push(String::new());
        }
        if self.cursor_row >= self.lines.len() {
            self.cursor_row = self.lines.len().saturating_sub(1);
        }
        let len = self.current_line_len();
        if self.cursor_col > len {
            self.cursor_col = len;
        }
    }

    fn short_path(&self) -> String {
        if let Ok(cwd) = std::env::current_dir() {
            if let Ok(stripped) = self.path.strip_prefix(&cwd) {
                return stripped.display().to_string();
            }
        }
        self.path.display().to_string()
    }

    fn cursor_col(&self) -> usize {
        self.cursor_col
    }

    fn relative_cursor_row(&self) -> usize {
        self.cursor_row.saturating_sub(self.scroll_row)
    }

    fn adjust_scroll(&mut self, viewport_height: usize) {
        if viewport_height == 0 {
            return;
        }
        if self.cursor_row < self.scroll_row {
            self.scroll_row = self.cursor_row;
        }
        let max_visible_row = self.scroll_row + viewport_height - 1;
        if self.cursor_row > max_visible_row {
            self.scroll_row = self.cursor_row + 1 - viewport_height;
        }
    }

    fn visible_text(&self, viewport_height: usize) -> String {
        if viewport_height == 0 {
            return String::new();
        }
        let end = (self.scroll_row + viewport_height).min(self.lines.len());
        self.lines[self.scroll_row..end].join("\n")
    }

    fn handle_key(&mut self, key: KeyEvent) -> Result<EditorAction> {
        if key.modifiers.contains(KeyModifiers::CONTROL) {
            if let KeyCode::Char(ch) = key.code {
                if matches!(ch, 's' | 'S') {
                    self.save()?;
                    self.pending_status = Some(format!("Saved {}", self.short_path()));
                    return Ok(EditorAction::Continue);
                }
            }
        }

        match key.code {
            KeyCode::Esc => {
                if self.dirty {
                    self.save()?;
                    self.pending_status = Some(format!("Saved {}", self.short_path()));
                }
                return Ok(EditorAction::Exit);
            }
            KeyCode::Enter => self.insert_newline(),
            KeyCode::Backspace => self.backspace(),
            KeyCode::Delete => self.delete_forward(),
            KeyCode::Left => self.move_left(),
            KeyCode::Right => self.move_right(),
            KeyCode::Up => self.move_up(),
            KeyCode::Down => self.move_down(),
            KeyCode::Home => self.move_line_start(),
            KeyCode::End => self.move_line_end(),
            KeyCode::Char(ch) => {
                if !key.modifiers.contains(KeyModifiers::CONTROL)
                    && !key.modifiers.contains(KeyModifiers::ALT)
                {
                    self.insert_char(ch);
                }
            }
            _ => {}
        }

        Ok(EditorAction::Continue)
    }

    fn take_status_message(&mut self) -> Option<String> {
        self.pending_status.take()
    }

    fn save(&mut self) -> Result<()> {
        let contents = self.lines.join("\n");
        fs::write(&self.path, contents)?;
        self.dirty = false;
        self.last_disk_mtime = Self::disk_mtime(&self.path);
        Ok(())
    }

    fn current_line_len(&self) -> usize {
        self.lines
            .get(self.cursor_row)
            .map(|line| line.len())
            .unwrap_or(0)
    }

    fn insert_char(&mut self, ch: char) {
        if let Some(line) = self.lines.get_mut(self.cursor_row) {
            line.insert(self.cursor_col, ch);
            self.cursor_col += 1;
            self.dirty = true;
        }
    }

    fn insert_newline(&mut self) {
        if let Some(line) = self.lines.get_mut(self.cursor_row) {
            let trailing = line.split_off(self.cursor_col);
            self.lines.insert(self.cursor_row + 1, trailing);
            self.cursor_row += 1;
            self.cursor_col = 0;
            self.dirty = true;
        }
    }

    fn backspace(&mut self) {
        if self.cursor_col > 0 {
            if let Some(line) = self.lines.get_mut(self.cursor_row) {
                self.cursor_col -= 1;
                line.remove(self.cursor_col);
                self.dirty = true;
            }
        } else if self.cursor_row > 0 {
            let current_line = self.lines.remove(self.cursor_row);
            self.cursor_row -= 1;
            if let Some(prev) = self.lines.get_mut(self.cursor_row) {
                let prev_len = prev.len();
                prev.push_str(&current_line);
                self.cursor_col = prev_len;
            } else {
                self.cursor_col = 0;
            }
            self.dirty = true;
        }
    }

    fn delete_forward(&mut self) {
        if let Some(line) = self.lines.get_mut(self.cursor_row) {
            if self.cursor_col < line.len() {
                line.remove(self.cursor_col);
                self.dirty = true;
                return;
            }
        }
        if self.cursor_row + 1 < self.lines.len() {
            let next_line = self.lines.remove(self.cursor_row + 1);
            if let Some(line) = self.lines.get_mut(self.cursor_row) {
                line.push_str(&next_line);
            }
            self.dirty = true;
        }
    }

    fn move_left(&mut self) {
        if self.cursor_col > 0 {
            self.cursor_col -= 1;
        } else if self.cursor_row > 0 {
            self.cursor_row -= 1;
            self.cursor_col = self.current_line_len();
        }
    }

    fn move_right(&mut self) {
        let len = self.current_line_len();
        if self.cursor_col < len {
            self.cursor_col += 1;
        } else if self.cursor_row + 1 < self.lines.len() {
            self.cursor_row += 1;
            self.cursor_col = 0;
        }
    }

    fn move_up(&mut self) {
        if self.cursor_row > 0 {
            self.cursor_row -= 1;
            let len = self.current_line_len();
            self.cursor_col = self.cursor_col.min(len);
        }
    }

    fn move_down(&mut self) {
        if self.cursor_row + 1 < self.lines.len() {
            self.cursor_row += 1;
            let len = self.current_line_len();
            self.cursor_col = self.cursor_col.min(len);
        }
    }

    fn move_line_start(&mut self) {
        self.cursor_col = 0;
    }

    fn move_line_end(&mut self) {
        self.cursor_col = self.current_line_len();
    }
}

fn trim_logs(logs: &mut VecDeque<LogRecord>, max: usize) {
    while logs.len() > max {
        logs.pop_front();
    }
}

fn logs_to_text(entries: &VecDeque<LogRecord>, max_lines: usize) -> Text<'static> {
    if max_lines == 0 {
        return Text::from("");
    }
    let start = entries.len().saturating_sub(max_lines);
    let mut lines = Vec::new();
    for record in entries.iter().skip(start) {
        let mut spans = Vec::new();
        spans.push(Span::styled(
            record.source.label(),
            record.source.style(),
        ));
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
                while let Some(next) = chars.next() {
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
        .map(|part| if part.is_empty() { 0 } else { part.parse().unwrap_or(0) })
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
                                *style = if is_fg { style.fg(color) } else { style.bg(color) };
                            }
                        }
                        2 => {
                            let r = iter.next().unwrap_or(0) as u8;
                            let g = iter.next().unwrap_or(0) as u8;
                            let b = iter.next().unwrap_or(0) as u8;
                            let color = Color::Rgb(r, g, b);
                            *style = if is_fg { style.fg(color) } else { style.bg(color) };
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
        0 => if bright { Color::DarkGray } else { Color::Black },
        1 => if bright { Color::LightRed } else { Color::Red },
        2 => if bright { Color::LightGreen } else { Color::Green },
        3 => if bright { Color::LightYellow } else { Color::Yellow },
        4 => if bright { Color::LightBlue } else { Color::Blue },
        5 => if bright { Color::LightMagenta } else { Color::Magenta },
        6 => if bright { Color::LightCyan } else { Color::Cyan },
        7 => if bright { Color::White } else { Color::Gray },
        _ => return None,
    };
    Some(color)
}

#[derive(Clone)]
struct LogRecord {
    source: LogSource,
    content: String,
}

impl LogRecord {
    fn new(source: LogSource, content: String) -> Self {
        Self { source, content }
    }
}

#[derive(Clone, Copy)]
enum LogSource {
    Stdout,
    Stderr,
}

impl LogSource {
    fn label(self) -> &'static str {
        match self {
            LogSource::Stdout => "[out]",
            LogSource::Stderr => "[err]",
        }
    }

    fn style(self) -> Style {
        match self {
            LogSource::Stdout => Style::default().fg(Color::LightGreen),
            LogSource::Stderr => Style::default().fg(Color::LightRed),
        }
    }
}

fn spawn_log_reader<R: Read + Send + 'static>(
    stream: R,
    source: LogSource,
    logs: Arc<Mutex<VecDeque<LogRecord>>>,
    max_logs: usize,
) {
    thread::spawn(move || {
        let mut reader = BufReader::new(stream);
        let mut buffer = [0u8; 1024];
        let mut pending = String::new();
        loop {
            match reader.read(&mut buffer) {
                Ok(0) => {
                    if !pending.is_empty() {
                        push_log_line(&logs, source, &pending, max_logs);
                        pending.clear();
                    }
                    break;
                }
                Ok(n) => {
                    pending.push_str(&String::from_utf8_lossy(&buffer[..n]));
                    while let Some(idx) = pending.find(|c| c == '\n' || c == '\r') {
                        let line = pending[..idx].to_string();
                        push_log_line(&logs, source, &line, max_logs);
                        let delim = pending.as_bytes()[idx];
                        let mut consume = idx + 1;
                        if delim == b'\r' {
                            if pending.as_bytes().get(consume) == Some(&b'\n') {
                                consume += 1;
                            }
                        } else if delim == b'\n' {
                            if pending.as_bytes().get(consume) == Some(&b'\r') {
                                consume += 1;
                            }
                        }
                        pending.drain(..consume);
                    }
                }
                Err(err) if err.kind() == io::ErrorKind::Interrupted => continue,
                Err(_) => break,
            }
        }
    });
}

fn push_log_line(
    logs: &Arc<Mutex<VecDeque<LogRecord>>>,
    source: LogSource,
    line: &str,
    max_logs: usize,
) {
    let mut guard = logs.lock().unwrap();
    guard.push_back(LogRecord::new(source, format!("{line}\n")));
    trim_logs(&mut guard, max_logs);
}

fn spawn_cli_child(
    cli_args: &[String],
    logs: Arc<Mutex<VecDeque<LogRecord>>>,
    max_logs: usize,
) -> Result<Child> {
    let exe = std::env::current_exe().context("locate current executable")?;
    let mut cmd = Command::new(exe);
    if !cli_args.is_empty() {
        cmd.args(cli_args);
    }
    cmd.env("CLICOLOR_FORCE", "1");
    cmd.env("FORCE_COLOR", "1");
    cmd.env("NO_COLOR", "0");
    cmd.env("OXBOOK_STREAM_STDOUT", "1");
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

    let mut child = cmd.spawn().context("spawn oxbook-cli child")?;

    if let Some(stdout) = child.stdout.take() {
        spawn_log_reader(stdout, LogSource::Stdout, logs.clone(), max_logs);
    }

    if let Some(stderr) = child.stderr.take() {
        spawn_log_reader(stderr, LogSource::Stderr, logs.clone(), max_logs);
    }

    Ok(child)
}

