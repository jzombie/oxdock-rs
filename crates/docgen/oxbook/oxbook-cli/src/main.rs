use anyhow::{Context, Result};
use crossterm::{
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event as CEvent, KeyCode, KeyEvent,
        KeyEventKind, KeyModifiers, MouseButton, MouseEventKind,
    },
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::{Backend, CrosstermBackend},
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Span, Spans, Text},
    widgets::{Block, Borders, Paragraph},
    Frame, Terminal,
};
use std::collections::VecDeque;
use std::fs;
use std::io::{self, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::{mpsc, Arc, Mutex};
use std::thread;
use std::time::{Duration, SystemTime};
use oxbook_cli::WORKER_EVENT_PREFIX;

const RUN_GLYPH_IDLE: &str = "[>]";
const RUN_GLYPH_RUNNING: &str = "[X]";
const RUN_GLYPH_BLANK: &str = "   ";
const RUN_GLYPH_WIDTH: usize = RUN_GLYPH_IDLE.len();

fn main() -> Result<()> {
    enum LaunchMode {
        Default,
        Tui,
        RunBlock { path: String, line: usize },
    }

    let mut mode = LaunchMode::Default;
    let mut cli_args = Vec::new();
    let mut args = std::env::args().skip(1).peekable();
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--tui" | "-t" => mode = LaunchMode::Tui,
            "tui" if matches!(mode, LaunchMode::Default) && cli_args.is_empty() => {
                mode = LaunchMode::Tui
            }
            "--run-block" | "run-block" => {
                let path = args
                    .next()
                    .context("expected path after --run-block")?;
                let line = args
                    .next()
                    .context("expected line number after --run-block")?
                    .parse::<usize>()
                    .context("invalid line number for --run-block")?;
                mode = LaunchMode::RunBlock { path, line };
            }
            _ => cli_args.push(arg),
        }
    }

    match mode {
        LaunchMode::Tui => run_tui(cli_args),
        LaunchMode::RunBlock { path, line } => oxbook_cli::run_block(&path, line),
        LaunchMode::Default => oxbook_cli::run(),
    }
}

fn run_tui(cli_args: Vec<String>) -> Result<()> {
    const MAX_LOG_LINES: usize = 800;
    let target_path = resolve_target(&cli_args);

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.clear()?;

    let result = (|| -> Result<()> {
        let logs: Arc<Mutex<VecDeque<LogRecord>>> = Arc::new(Mutex::new(VecDeque::new()));
        let mut status = String::from("Idle");
        let mut server: Option<ServerProcess> = None;
        let (worker_event_tx, worker_event_rx) = mpsc::channel();
        let mut running_line: Option<usize> = None;
        let mut mode = UiMode::Dashboard;

        loop {
            if let Some(proc) = server.as_mut() {
                if let Some(exit) = proc.child.try_wait()? {
                    let mut guard = logs.lock().unwrap();
                    guard.push_back(LogRecord::new(
                        LogSource::Stdout,
                        format!("process exited: {}\n", exit),
                    ));
                    trim_logs(&mut guard, MAX_LOG_LINES);
                    server = None;
                    status = String::from("Idle");
                    running_line = None;
                }
            }

            while let Ok(event) = worker_event_rx.try_recv() {
                match event {
                    WorkerEvent::Started(line) => {
                        running_line = Some(line.saturating_sub(1));
                        status = format!("Running block at line {}...", line);
                    }
                    WorkerEvent::Done { line, success } => {
                        running_line = None;
                        status = if success {
                            format!("Block at line {} completed", line)
                        } else {
                            format!("Block at line {} failed", line)
                        };
                    }
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
                    .margin(0)
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
                let mut header_view = HeaderView::new(header_text);
                header_view.render(f, layout[0]);

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
                        let mut controls_view = ControlsView::new(format!(
                            "Watch + Logs\n\n{target_info}\nPress 'e' to open the inline editor, 'r' to run oxbook-cli within this session.\nIn the editor view, click [>] to run a code block."
                        ));
                        controls_view.render(f, mid[0]);
                    }
                    UiMode::Editor(editor) => {
                        let mut editor_view = EditorView::new(editor, running_line);
                        editor_view.render(f, mid[0]);
                    }
                }

                let mut logs_view = LogsView::new(&logs);
                logs_view.render(f, mid[1]);

                let mut status_view = StatusView::new(status.clone());
                status_view.render(f, layout[2]);
            })?;

            if event::poll(Duration::from_millis(120))? {
                match event::read()? {
                    CEvent::Mouse(mouse_event) => {
                        if let UiMode::Editor(editor) = &mut mode {
                            if matches!(mouse_event.kind, MouseEventKind::Down(MouseButton::Left)) {
                                if let Some(line_idx) = editor
                                    .hit_test_run_glyph(mouse_event.column, mouse_event.row)
                                {
                                    let line_number = line_idx + 1;
                                    if let Some(server_proc) = server.as_mut() {
                                        if let Err(err) = server_proc.send_run_command(line_number) {
                                            let mut guard = logs.lock().unwrap();
                                            guard.push_back(LogRecord::new(
                                                LogSource::Stderr,
                                                format!(
                                                    "run-block error at line {}: {}\n",
                                                    line_number, err
                                                ),
                                            ));
                                            trim_logs(&mut guard, MAX_LOG_LINES);
                                            status = format!(
                                                "Snippet start failed at line {}",
                                                line_number
                                            );
                                        } else {
                                            let mut guard = logs.lock().unwrap();
                                            guard.push_back(LogRecord::new(
                                                LogSource::Stdout,
                                                format!(
                                                    "running block at line {}\n",
                                                    line_number
                                                ),
                                            ));
                                            trim_logs(&mut guard, MAX_LOG_LINES);
                                            running_line = Some(line_idx);
                                            status = format!(
                                                "Running block at line {}...",
                                                line_number
                                            );
                                        }
                                    } else {
                                        let mut guard = logs.lock().unwrap();
                                        guard.push_back(LogRecord::new(
                                            LogSource::Stdout,
                                            format!(
                                                "server not running; cannot run block at line {}\n",
                                                line_number
                                            ),
                                        ));
                                        trim_logs(&mut guard, MAX_LOG_LINES);
                                        status = String::from(
                                            "Start the server (press 'r') before running blocks",
                                        );
                                    }
                                }
                            }
                        }
                    }
                    CEvent::Key(key_event) => {
                        if key_event.kind != KeyEventKind::Press {
                            continue;
                        }
                        match &mut mode {
                            UiMode::Dashboard => match key_event.code {
                                KeyCode::Char('q') => {
                                    if let Some(mut proc) = server.take() {
                                        let _ = proc.child.kill();
                                        let _ = proc.child.wait();
                                    }
                                    break Ok(());
                                }
                                KeyCode::Char('r') => {
                                    if server.is_some() {
                                        status = String::from("Already running");
                                        continue;
                                    }
                                    match spawn_cli_child(
                                        &cli_args,
                                        logs.clone(),
                                        MAX_LOG_LINES,
                                        worker_event_tx.clone(),
                                    ) {
                                        Ok(new_child) => {
                                            status = String::from("Running...");
                                            server = Some(new_child);
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
                    _ => {}
                }
            }
        }
    })();

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen, DisableMouseCapture)?;
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

trait FramedView {
    fn block<'a>(&'a self) -> Block<'a>;
    fn render_inner<B: Backend>(&mut self, frame: &mut Frame<'_, B>, inner: Rect);

    fn render<B: Backend>(&mut self, frame: &mut Frame<'_, B>, area: Rect) {
        let block = self.block();
        let inner = block.inner(area);
        frame.render_widget(block, area);
        self.render_inner(frame, inner);
    }
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
    code_blocks: Vec<CodeBlockMeta>,
    code_blocks_dirty: bool,
    render_info: Option<EditorRenderInfo>,
}

struct LineRender {
    display: String,
    prefix_width: usize,
}

struct VisibleLines {
    lines: Vec<LineRender>,
    digits_width: usize,
    arrow_width: usize,
}

#[derive(Clone)]
struct CodeBlockMeta {
    fence_line: usize,
    _close_line: usize,
    _info: String,
}

struct EditorRenderInfo {
    inner_x: u16,
    inner_y: u16,
    inner_width: u16,
    inner_height: u16,
    prefix_width: usize,
    digits_width: usize,
    arrow_width: usize,
}

#[derive(Debug, Clone)]
enum WorkerEvent {
    Started(usize),
    Done { line: usize, success: bool },
}

struct ServerProcess {
    child: Child,
    stdin: Option<ChildStdin>,
}

impl ServerProcess {
    fn send_run_command(&mut self, line: usize) -> io::Result<()> {
        if let Some(stdin) = &mut self.stdin {
            writeln!(stdin, "RUN {line}")?;
            stdin.flush()
        } else {
            Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "server stdin unavailable",
            ))
        }
    }
}

impl EditorState {
    fn load(path: PathBuf) -> Result<Self> {
        let (lines, last_disk_mtime) = Self::read_disk_snapshot(&path)?;
        let code_blocks = Self::compute_code_blocks(&lines);
        Ok(Self {
            path,
            lines,
            cursor_row: 0,
            cursor_col: 0,
            scroll_row: 0,
            dirty: false,
            last_disk_mtime,
            pending_status: None,
            code_blocks,
            code_blocks_dirty: false,
            render_info: None,
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
        self.code_blocks = Self::compute_code_blocks(&self.lines);
        self.code_blocks_dirty = false;
        self.render_info = None;
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

    fn visible_lines_for_render(
        &mut self,
        viewport_height: usize,
        running_block: Option<usize>,
    ) -> VisibleLines {
        if viewport_height == 0 {
            return VisibleLines {
                lines: Vec::new(),
                digits_width: digits(self.lines.len().max(1)),
                arrow_width: RUN_GLYPH_WIDTH,
            };
        }

        self.ensure_code_blocks();

        let total_lines = self.lines.len().max(1);
        let digits_width = digits(total_lines);
        let end = (self.scroll_row + viewport_height).min(self.lines.len());
        let mut rendered = Vec::with_capacity(end.saturating_sub(self.scroll_row));
        for idx in self.scroll_row..end {
            let number = idx + 1;
            let arrow = if self.is_code_block_start(idx) {
                if Some(idx) == running_block {
                    RUN_GLYPH_RUNNING
                } else {
                    RUN_GLYPH_IDLE
                }
            } else {
                RUN_GLYPH_BLANK
            };
            let prefix = format!("{:>width$} {} | ", number, arrow, width = digits_width);
            let display = format!("{prefix}{}", self.lines[idx]);
            rendered.push(LineRender {
                display,
                prefix_width: prefix.len(),
            });
        }

        VisibleLines {
            lines: rendered,
            digits_width,
            arrow_width: RUN_GLYPH_WIDTH,
        }
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
            self.invalidate_layout();
        }
    }

    fn insert_newline(&mut self) {
        if let Some(line) = self.lines.get_mut(self.cursor_row) {
            let trailing = line.split_off(self.cursor_col);
            self.lines.insert(self.cursor_row + 1, trailing);
            self.cursor_row += 1;
            self.cursor_col = 0;
            self.dirty = true;
            self.invalidate_layout();
        }
    }

    fn backspace(&mut self) {
        if self.cursor_col > 0 {
            if let Some(line) = self.lines.get_mut(self.cursor_row) {
                self.cursor_col -= 1;
                line.remove(self.cursor_col);
                self.dirty = true;
                self.invalidate_layout();
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
            self.invalidate_layout();
        }
    }

    fn delete_forward(&mut self) {
        if let Some(line) = self.lines.get_mut(self.cursor_row) {
            if self.cursor_col < line.len() {
                line.remove(self.cursor_col);
                self.dirty = true;
                self.invalidate_layout();
                return;
            }
        }
        if self.cursor_row + 1 < self.lines.len() {
            let next_line = self.lines.remove(self.cursor_row + 1);
            if let Some(line) = self.lines.get_mut(self.cursor_row) {
                line.push_str(&next_line);
            }
            self.dirty = true;
            self.invalidate_layout();
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

    fn hit_test_run_glyph(&self, column: u16, row: u16) -> Option<usize> {
        let info = self.render_info.as_ref()?;
        if column < info.inner_x || row < info.inner_y {
            return None;
        }

        let rel_x = (column - info.inner_x) as usize;
        let rel_y = (row - info.inner_y) as usize;
        let inner_width = info.inner_width as usize;
        let inner_height = info.inner_height as usize;
        if rel_x >= inner_width || rel_y >= inner_height {
            return None;
        }

        if rel_x >= info.prefix_width {
            return None;
        }

        let arrow_start = info.digits_width + 1;
        if rel_x < arrow_start || rel_x >= arrow_start + info.arrow_width {
            return None;
        }

        let line_idx = self.scroll_row + rel_y;
        if line_idx >= self.lines.len() {
            return None;
        }
        if self.is_code_block_start(line_idx) {
            Some(line_idx)
        } else {
            None
        }
    }

    fn invalidate_layout(&mut self) {
        self.code_blocks_dirty = true;
        self.render_info = None;
    }

    fn ensure_code_blocks(&mut self) {
        if self.code_blocks_dirty {
            self.code_blocks = Self::compute_code_blocks(&self.lines);
            self.code_blocks_dirty = false;
        }
    }

    fn is_code_block_start(&self, line_idx: usize) -> bool {
        self.code_blocks
            .iter()
            .any(|block| block.fence_line == line_idx)
    }

    fn compute_code_blocks(lines: &[String]) -> Vec<CodeBlockMeta> {
        #[derive(Clone)]
        struct OpenBlock {
            start: usize,
            fence_char: char,
            fence_len: usize,
            info: String,
        }

        let mut blocks = Vec::new();
        let mut current: Option<OpenBlock> = None;

        for (idx, line) in lines.iter().enumerate() {
            let marker = Self::parse_fence_marker(line);
            if let Some(open) = current.as_ref() {
                if let Some((_, fence_char, fence_len, info)) = marker.as_ref() {
                    if *fence_char == open.fence_char
                        && *fence_len >= open.fence_len
                        && info.is_empty()
                    {
                        blocks.push(CodeBlockMeta {
                            fence_line: open.start,
                            _close_line: idx,
                            _info: open.info.clone(),
                        });
                        current = None;
                        continue;
                    }
                }
            }

            if current.is_none() {
                if let Some((_, fence_char, fence_len, info)) = marker {
                    current = Some(OpenBlock {
                        start: idx,
                        fence_char,
                        fence_len,
                        info,
                    });
                }
            }
        }

        if let Some(open) = current {
            blocks.push(CodeBlockMeta {
                fence_line: open.start,
                _close_line: lines.len().saturating_sub(1),
                _info: open.info,
            });
        }

        blocks
    }

    fn parse_fence_marker(line: &str) -> Option<(usize, char, usize, String)> {
        let bytes = line.as_bytes();
        let mut idx = 0usize;
        let mut ws = 0usize;
        while idx < bytes.len() && (bytes[idx] == b' ' || bytes[idx] == b'\t') {
            ws += 1;
            idx += 1;
        }
        if ws > 3 || idx >= bytes.len() {
            return None;
        }

        let fence_char = bytes[idx] as char;
        if fence_char != '`' && fence_char != '~' {
            return None;
        }
        idx += 1;
        let mut fence_len = 1;
        while idx < bytes.len() && bytes[idx] as char == fence_char {
            fence_len += 1;
            idx += 1;
        }
        if fence_len < 3 {
            return None;
        }

        let rest = &line[idx..];
        Some((ws, fence_char, fence_len, rest.trim().to_string()))
    }
}


struct HeaderView<'a> {
    text: &'a str,
}

impl<'a> HeaderView<'a> {
    fn new(text: &'a str) -> Self {
        Self { text }
    }
}

impl<'a> FramedView for HeaderView<'a> {
    fn block<'b>(&'b self) -> Block<'b> {
        Block::default().borders(Borders::ALL).title("oxbook-cli")
    }

    fn render_inner<B: Backend>(&mut self, frame: &mut Frame<'_, B>, inner: Rect) {
        let widget = Paragraph::new(self.text).alignment(Alignment::Center);
        frame.render_widget(widget, inner);
    }
}

struct ControlsView {
    body: String,
}

impl ControlsView {
    fn new(body: String) -> Self {
        Self { body }
    }
}

impl FramedView for ControlsView {
    fn block<'a>(&'a self) -> Block<'a> {
        Block::default().borders(Borders::ALL).title("Controls")
    }

    fn render_inner<B: Backend>(&mut self, frame: &mut Frame<'_, B>, inner: Rect) {
        let widget = Paragraph::new(self.body.as_str());
        frame.render_widget(widget, inner);
    }
}

struct EditorView<'a> {
    editor: &'a mut EditorState,
    running_block: Option<usize>,
}

impl<'a> EditorView<'a> {
    fn new(editor: &'a mut EditorState, running_block: Option<usize>) -> Self {
        Self {
            editor,
            running_block,
        }
    }
}

impl<'a> FramedView for EditorView<'a> {
    fn block<'b>(&'b self) -> Block<'b> {
        Block::default()
            .borders(Borders::ALL)
            .title(format!("Editing {}", self.editor.short_path()))
    }

    fn render_inner<B: Backend>(&mut self, frame: &mut Frame<'_, B>, inner: Rect) {
        if inner.width == 0 || inner.height == 0 {
            return;
        }

        let viewport = inner.height as usize;
        self.editor.adjust_scroll(viewport);
        let visible = self
            .editor
            .visible_lines_for_render(viewport, self.running_block);
        let text = visible
            .lines
            .iter()
            .map(|line| line.display.as_str())
            .collect::<Vec<_>>()
            .join("\n");
        frame.render_widget(Paragraph::new(text), inner);

        self.editor.render_info = Some(EditorRenderInfo {
            inner_x: inner.x,
            inner_y: inner.y,
            inner_width: inner.width,
            inner_height: inner.height,
            prefix_width: visible
                .lines
                .first()
                .map(|line| line.prefix_width)
                .unwrap_or(0),
            digits_width: visible.digits_width,
            arrow_width: visible.arrow_width,
        });

        let cursor_row = self
            .editor
            .relative_cursor_row()
            .min(visible.lines.len().saturating_sub(1));
        if let Some(line) = visible.lines.get(cursor_row) {
            let inner_width = inner.width as usize;
            let prefix = line.prefix_width.min(inner_width);
            let prefix_cols = prefix as u16;
            if inner_width > prefix {
                let content_width = inner_width.saturating_sub(prefix);
                if content_width > 0 {
                    let max_col = content_width.saturating_sub(1);
                    let cursor_col = self.editor.cursor_col().min(max_col);
                    frame.set_cursor(
                        inner.x + prefix_cols + cursor_col as u16,
                        inner.y + cursor_row as u16,
                    );
                }
            }
        }
    }
}

struct LogsView<'a> {
    logs: &'a Arc<Mutex<VecDeque<LogRecord>>>,
}

impl<'a> LogsView<'a> {
    fn new(logs: &'a Arc<Mutex<VecDeque<LogRecord>>>) -> Self {
        Self { logs }
    }
}

impl<'a> FramedView for LogsView<'a> {
    fn block<'b>(&'b self) -> Block<'b> {
        Block::default().borders(Borders::ALL).title("Logs")
    }

    fn render_inner<B: Backend>(&mut self, frame: &mut Frame<'_, B>, inner: Rect) {
        let text = {
            let guard = self.logs.lock().unwrap();
            if guard.is_empty() {
                Text::from("(no logs yet)")
            } else {
                let height = inner.height as usize;
                logs_to_text(&guard, height.max(1))
            }
        };
        frame.render_widget(Paragraph::new(text), inner);
    }
}

struct StatusView {
    message: String,
}

impl StatusView {
    fn new(message: String) -> Self {
        Self { message }
    }
}

impl FramedView for StatusView {
    fn block<'a>(&'a self) -> Block<'a> {
        Block::default().borders(Borders::ALL).title("Status")
    }

    fn render_inner<B: Backend>(&mut self, frame: &mut Frame<'_, B>, inner: Rect) {
        let widget = Paragraph::new(self.message.clone())
            .style(Style::default().add_modifier(Modifier::BOLD));
        frame.render_widget(widget, inner);
    }
}

fn trim_logs(logs: &mut VecDeque<LogRecord>, max: usize) {
    while logs.len() > max {
        logs.pop_front();
    }
}

fn digits(mut value: usize) -> usize {
    let mut width = 1;
    while value >= 10 {
        value /= 10;
        width += 1;
    }
    width
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
    event_tx: mpsc::Sender<WorkerEvent>,
) -> Result<ServerProcess> {
    spawn_cli_command(cli_args.to_vec(), logs, max_logs, Some(event_tx))
}

fn spawn_cli_command(
    args: Vec<String>,
    logs: Arc<Mutex<VecDeque<LogRecord>>>,
    max_logs: usize,
    event_tx: Option<mpsc::Sender<WorkerEvent>>,
) -> Result<ServerProcess> {
    let exe = std::env::current_exe().context("locate current executable")?;
    let mut cmd = Command::new(exe);
    if !args.is_empty() {
        cmd.args(&args);
    }
    cmd.env("CLICOLOR_FORCE", "1");
    cmd.env("FORCE_COLOR", "1");
    cmd.env("NO_COLOR", "0");
    cmd.env("OXBOOK_STREAM_STDOUT", "1");
    cmd.stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = cmd.spawn().context("spawn oxbook-cli child")?;

    if let Some(stdout) = child.stdout.take() {
        spawn_log_reader(
            stdout,
            LogSource::Stdout,
            logs.clone(),
            max_logs,
            event_tx.clone(),
        );
    }

    if let Some(stderr) = child.stderr.take() {
        spawn_log_reader(stderr, LogSource::Stderr, logs.clone(), max_logs, None);
    }

    let stdin = child
        .stdin
        .take()
        .context("obtain stdin for oxbook-cli child")?;

    Ok(ServerProcess {
        child,
        stdin: Some(stdin),
    })
}

fn spawn_log_reader<R: Read + Send + 'static>(
    stream: R,
    source: LogSource,
    logs: Arc<Mutex<VecDeque<LogRecord>>>,
    max_logs: usize,
    event_tx: Option<mpsc::Sender<WorkerEvent>>,
) {
    thread::spawn(move || {
        let mut reader = BufReader::new(stream);
        let mut buffer = [0u8; 1024];
        let mut pending = String::new();
        loop {
            match reader.read(&mut buffer) {
                Ok(0) => {
                    if !pending.is_empty() {
                        dispatch_worker_line(
                            &pending,
                            &logs,
                            max_logs,
                            source,
                            event_tx.as_ref(),
                        );
                        pending.clear();
                    }
                    break;
                }
                Ok(n) => {
                    pending.push_str(&String::from_utf8_lossy(&buffer[..n]));
                    while let Some(idx) = pending.find(|c| c == '\n' || c == '\r') {
                        let line = pending[..idx].to_string();
                        dispatch_worker_line(
                            &line,
                            &logs,
                            max_logs,
                            source,
                            event_tx.as_ref(),
                        );
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

fn dispatch_worker_line(
    line: &str,
    logs: &Arc<Mutex<VecDeque<LogRecord>>>,
    max_logs: usize,
    source: LogSource,
    event_tx: Option<&mpsc::Sender<WorkerEvent>>,
) {
    if let Some(rest) = line.strip_prefix(WORKER_EVENT_PREFIX) {
        if let Some(tx) = event_tx {
            let mut parts = rest.trim_start().split_whitespace();
            match parts.next() {
                Some("START") => {
                    if let Some(line_num) =
                        parts.next().and_then(|value| value.parse::<usize>().ok())
                    {
                        let _ = tx.send(WorkerEvent::Started(line_num));
                    }
                }
                Some("DONE") => {
                    if let Some(line_num) =
                        parts.next().and_then(|value| value.parse::<usize>().ok())
                    {
                        let success = matches!(parts.next(), Some("OK"));
                        let _ = tx.send(WorkerEvent::Done {
                            line: line_num,
                            success,
                        });
                    }
                }
                _ => {}
            }
        }
        return;
    }

    push_log_line(logs, source, line, max_logs);
}

