use anyhow::{anyhow, Context, Result};
use arboard::Clipboard;
use base64::engine::general_purpose::STANDARD as BASE64_STD;
use base64::Engine as _;
use comrak::{Arena, Options, nodes::NodeValue, parse_document};
use crossterm::{
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event as CEvent, KeyCode, KeyEvent,
        KeyEventKind, KeyModifiers, KeyboardEnhancementFlags, MouseButton, MouseEventKind,
        PopKeyboardEnhancementFlags, PushKeyboardEnhancementFlags,
    },
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use ratatui::{
    Frame, Terminal,
    backend::{Backend, CrosstermBackend},
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Span, Spans, Text},
    widgets::{Block, Borders, Paragraph},
};
use runbook_cli::session::{start_session, SessionConfig, SessionEvent, SessionHandle, SessionLogSource};
use std::{
    collections::{HashMap, VecDeque, hash_map::DefaultHasher},
    fs,
    hash::{Hash, Hasher},
    io,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
    time::{Duration, SystemTime},
};

const STATUS_ICON_READY: &str = "○";
const STATUS_ICON_RUNNING: &str = "●";
const STATUS_ICON_DISABLED: &str = "◌";
const STATUS_ICON_BLANK: &str = " ";
const RUN_BUTTON_READY: &str = "▶";
const RUN_BUTTON_RUNNING: &str = "■";
const RUN_BUTTON_DISABLED: &str = "·";
const RUN_BUTTON_BLANK: &str = " ";
const RUN_BUTTON_WIDTH: usize = 1;
const MAX_LOG_LINES: usize = 800;
const DEFAULT_SCROLL_LINES_PER_TICK: isize = 6;
const SELECTION_FG: Color = Color::Black;
const SELECTION_BG: Color = Color::LightCyan;

struct TuiConfig {
    scroll_lines_per_tick: isize,
}

impl Default for TuiConfig {
    fn default() -> Self {
        Self {
            scroll_lines_per_tick: DEFAULT_SCROLL_LINES_PER_TICK,
        }
    }
}

#[derive(Clone, Copy)]
struct KeyBinding {
    code: KeyCode,
    modifiers: KeyModifiers,
}

impl KeyBinding {
    fn new(code: KeyCode, modifiers: KeyModifiers) -> Self {
        Self { code, modifiers }
    }

    fn matches(&self, event: &KeyEvent) -> bool {
        event.code == self.code && event.modifiers == self.modifiers
    }
}

#[derive(Clone, Copy)]
struct KeyBindings {
    dashboard_quit: KeyBinding,
    dashboard_open_editor: KeyBinding,
    dashboard_ready: KeyBinding,
    editor_run_block: KeyBinding,
}

impl KeyBindings {
    fn default() -> Self {
        let none = KeyModifiers::empty();
        Self {
            dashboard_quit: KeyBinding::new(KeyCode::Char('q'), none),
            dashboard_open_editor: KeyBinding::new(KeyCode::Char('e'), none),
            dashboard_ready: KeyBinding::new(KeyCode::Char('r'), none),
            editor_run_block: KeyBinding::new(KeyCode::Char('r'), KeyModifiers::CONTROL),
        }
    }
}

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
                mode = LaunchMode::Tui;
            }
            "--run-block" | "run-block" => {
                let path = args.next().context("expected path after --run-block")?;
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
        LaunchMode::RunBlock { path, line } => runbook_cli::run_block(&path, line),
        LaunchMode::Default => runbook_cli::run(),
    }
}

fn run_tui(cli_args: Vec<String>) -> Result<()> {
    let config = TuiConfig::default();
    let target_path = resolve_target(&cli_args);
    let session_target = cli_args
        .first()
        .cloned()
        .unwrap_or_else(|| String::from("README.md"));

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    let keyboard_flags = KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES
        | KeyboardEnhancementFlags::REPORT_ALL_KEYS_AS_ESCAPE_CODES
        | KeyboardEnhancementFlags::REPORT_ALTERNATE_KEYS
        | KeyboardEnhancementFlags::REPORT_EVENT_TYPES;
    execute!(
        stdout,
        EnterAlternateScreen,
        EnableMouseCapture,
        PushKeyboardEnhancementFlags(keyboard_flags)
    )?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.clear()?;

    let result = (|| -> Result<()> {
        let logs: Arc<Mutex<VecDeque<LogRecord>>> = Arc::new(Mutex::new(VecDeque::new()));
        let mut status = String::from("Idle");
        let mut mode = UiMode::Dashboard;
        let (handle, events) = start_session(SessionConfig {
            target: session_target.clone(),
        })?;
        let session_handle = handle;
        let session_events = events;
        let mut running_line: Option<usize> = None;
        let keymap = KeyBindings::default();
        let mut ui_layout = UiLayout::default();
        let mut log_scroll_state = LogScrollState::default();
        let mut clipboard: Option<Clipboard> = None;

        loop {
            while let Some(event) = session_events.try_recv() {
                match event {
                    SessionEvent::RunStarted { line } => {
                        running_line = Some(line);
                        status = format!("Running block at line {}...", line);
                        push_log_line(
                            &logs,
                            LogSource::Stdout,
                            &format!("running block at line {}", line),
                            MAX_LOG_LINES,
                        );
                    }
                    SessionEvent::RunCompleted { line, success } => {
                        running_line = None;
                        if let UiMode::Editor(editor) = &mut mode {
                            editor.mark_disk_stale();
                        }
                        if success {
                            status = format!("Block at line {} completed", line);
                            push_log_line(
                                &logs,
                                LogSource::Stdout,
                                &format!("block at line {} completed", line),
                                MAX_LOG_LINES,
                            );
                        } else {
                            status = format!("Block at line {} failed", line);
                            push_log_line(
                                &logs,
                                LogSource::Stderr,
                                &format!("block at line {} failed", line),
                                MAX_LOG_LINES,
                            );
                        }
                    }
                    SessionEvent::RunFailed { line, error } => {
                        running_line = None;
                        if let UiMode::Editor(editor) = &mut mode {
                            editor.mark_disk_stale();
                        }
                        status = format!("Block at line {} failed", line);
                        push_log_line(
                            &logs,
                            LogSource::Stderr,
                            &format!("block at line {} error: {}", line, error),
                            MAX_LOG_LINES,
                        );
                    }
                    SessionEvent::Log { source, message } => {
                        let log_source = match source {
                            SessionLogSource::Stdout => LogSource::Stdout,
                            SessionLogSource::Stderr => LogSource::Stderr,
                        };
                        let line = if message.ends_with('\n') {
                            message
                        } else {
                            format!("{message}\n")
                        };
                        let mut guard = logs.lock().unwrap();
                        guard.push_back(LogRecord::new(log_source, line));
                        trim_logs(&mut guard, MAX_LOG_LINES);
                    }
                    SessionEvent::AutoRunQueued { line } => {
                        push_log_line(
                            &logs,
                            LogSource::Stdout,
                            &format!("queued auto-run for block at line {}", line),
                            MAX_LOG_LINES,
                        );
                    }
                    SessionEvent::AutoRunSkipped { line } => {
                        push_log_line(
                            &logs,
                            LogSource::Stdout,
                            &format!("block at line {} already queued", line),
                            MAX_LOG_LINES,
                        );
                    }
                    SessionEvent::Busy { requested, active } => {
                        status = String::from("Run in progress; wait for current block");
                        push_log_line(
                            &logs,
                            LogSource::Stdout,
                            &format!(
                                "run for line {} deferred; block {} is active",
                                requested, active
                            ),
                            MAX_LOG_LINES,
                        );
                    }
                    SessionEvent::StopQueued { line } => {
                        push_log_line(
                            &logs,
                            LogSource::Stdout,
                            &format!("stop will be requested once block at line {} starts", line),
                            MAX_LOG_LINES,
                        );
                        status =
                            format!("Stop queued for block at line {}; waiting to start", line);
                    }
                    SessionEvent::StopIssued { line } => {
                        push_log_line(
                            &logs,
                            LogSource::Stdout,
                            &format!("stop requested for block at line {}", line),
                            MAX_LOG_LINES,
                        );
                        status = format!("Stop requested for block at line {}", line);
                    }
                    SessionEvent::StopIgnored { line } => {
                        push_log_line(
                            &logs,
                            LogSource::Stdout,
                            &format!("no active run to stop at line {}", line),
                            MAX_LOG_LINES,
                        );
                    }
                }
            }

            if let UiMode::Editor(editor) = &mut mode {
                if let Some(message) = editor.sync_with_disk()? {
                    status = message;
                }
            }

            let running_line_idx = running_line.map(|line| line.saturating_sub(1));
            let can_run_blocks = running_line.is_none();

            let ui_layout_state = &mut ui_layout;
            let logs_scroll = &mut log_scroll_state;
            terminal.draw(|f| {
                ui_layout_state.reset();
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
                    UiMode::Dashboard => "RunBook TUI — 'e' edit • 'q' quit",
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
                            "Watch + Logs\n\n{target_info}\nPress 'e' to open the inline editor. In the editor view, click ▶ beside a code block or press Ctrl+R to run it. Use 'q' to quit."
                        ));
                        controls_view.render(f, mid[0]);
                        ui_layout_state.editor_area = None;
                    }
                    UiMode::Editor(editor) => {
                        let mut editor_view =
                            EditorView::new(editor, running_line_idx, can_run_blocks);
                        editor_view.render(f, mid[0]);
                        ui_layout_state.editor_area = Some(mid[0]);
                    }
                }

                let mut logs_view = LogsView::new(&logs, logs_scroll);
                logs_view.render(f, mid[1]);
                ui_layout_state.logs_area = Some(mid[1]);

                let mut status_view = StatusView::new(status.clone());
                status_view.render(f, layout[2]);
            })?;

            if event::poll(Duration::from_millis(120))? {
                match event::read()? {
                    CEvent::Mouse(mouse_event) => {
                        let mut handled = false;
                        if matches!(
                            mouse_event.kind,
                            MouseEventKind::ScrollUp | MouseEventKind::ScrollDown
                        ) {
                            if ui_layout.logs_contains(mouse_event.column, mouse_event.row) {
                                let delta = scroll_delta_for(mouse_event.kind, &config);
                                log_scroll_state.scroll(delta);
                                handled = true;
                            }
                        }
                        if handled {
                            continue;
                        }
                        if let UiMode::Editor(editor) = &mut mode {
                            match mouse_event.kind {
                                MouseEventKind::ScrollUp | MouseEventKind::ScrollDown => {
                                    if ui_layout
                                        .editor_contains(mouse_event.column, mouse_event.row)
                                    {
                                        if let Some(info) = editor.render_info.as_ref() {
                                            let viewport = info.inner_height as usize;
                                            let delta =
                                                scroll_delta_for(mouse_event.kind, &config);
                                            editor.scroll_by(delta, viewport);
                                        }
                                    }
                                }
                                MouseEventKind::Down(MouseButton::Left) => {
                                    if let Some(line_idx) = editor
                                        .hit_test_run_glyph(mouse_event.column, mouse_event.row)
                                    {
                                        let line_number = line_idx + 1;
                                        if run_block_via_session(
                                            editor,
                                            &session_handle,
                                            &logs,
                                            &mut status,
                                            running_line,
                                            line_number,
                                        ) {
                                            continue;
                                        }
                                    }

                                    if let Some(position) = editor
                                        .text_position_from_point(
                                            mouse_event.column,
                                            mouse_event.row,
                                        )
                                    {
                                        editor.begin_mouse_selection(position);
                                        log_selection_state(
                                            editor,
                                            &logs,
                                            "mouse-down selection",
                                        );
                                    } else {
                                        editor.clear_selection();
                                        editor.mouse_selecting = false;
                                        log_selection_state(
                                            editor,
                                            &logs,
                                            "mouse-down cleared selection",
                                        );
                                    }
                                }
                                MouseEventKind::Drag(MouseButton::Left) => {
                                    if editor.mouse_selecting {
                                        if let Some(position) = editor
                                            .text_position_from_point(
                                                mouse_event.column,
                                                mouse_event.row,
                                            )
                                        {
                                            editor.update_mouse_selection(position);
                                            log_selection_state(
                                                editor,
                                                &logs,
                                                "mouse-drag selection",
                                            );
                                        }
                                    }
                                }
                                MouseEventKind::Up(MouseButton::Left) => {
                                    if editor.mouse_selecting {
                                        editor.end_mouse_selection();
                                        log_selection_state(
                                            editor,
                                            &logs,
                                            "mouse-up selection",
                                        );
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    CEvent::Key(key_event) => {
                        if key_event.kind != KeyEventKind::Press {
                            continue;
                        }
                        match &mut mode {
                            UiMode::Dashboard => {
                                if keymap.dashboard_quit.matches(&key_event) {
                                    let _ = session_handle.shutdown();
                                    break Ok(());
                                } else if keymap.dashboard_open_editor.matches(&key_event) {
                                    match EditorState::load(target_path.clone()) {
                                        Ok(editor) => {
                                            status = format!("Editing {}", editor.short_path());
                                            mode = UiMode::Editor(editor);
                                        }
                                        Err(err) => {
                                            push_log_line(
                                                &logs,
                                                LogSource::Stderr,
                                                &format!("editor load error: {err}"),
                                                MAX_LOG_LINES,
                                            );
                                            status = String::from("Editor failed");
                                        }
                                    }
                                } else if keymap.dashboard_ready.matches(&key_event) {
                                    status = String::from(
                                        "Session ready; open the editor to run blocks",
                                    );
                                }
                            }
                            UiMode::Editor(editor) => {
                                if is_copy_shortcut(&key_event) {
                                    log_key_event(&logs, "copy shortcut", &key_event);
                                    // attempt system clipboard first, fallback to OSC52 for SSH/TTY clients
                                    let mut tried_clip = false;
                                    if clipboard.is_none() {
                                        match Clipboard::new() {
                                            Ok(new_clip) => clipboard = Some(new_clip),
                                            Err(err) => {
                                                push_log_line(
                                                    &logs,
                                                    LogSource::Stderr,
                                                    &format!(
                                                        "clipboard unavailable: {err}"
                                                    ),
                                                    MAX_LOG_LINES,
                                                );
                                                // continue to attempt OSC52 fallback below
                                            }
                                        }
                                    }
                                    if let Some(clip) = clipboard.as_mut() {
                                        tried_clip = true;
                                        match editor.copy_selection_to_clipboard(clip) {
                                            Ok(true) => {
                                                status = String::from(
                                                    "Selection copied to clipboard",
                                                );
                                                log_selection_state(
                                                    editor,
                                                    &logs,
                                                    "copy success",
                                                );
                                                continue;
                                            }
                                            Ok(false) => {
                                                // fallthrough to OSC52 attempt
                                                log_selection_state(
                                                    editor,
                                                    &logs,
                                                    "copy had no selection via system clipboard",
                                                );
                                            }
                                            Err(err) => {
                                                push_log_line(
                                                    &logs,
                                                    LogSource::Stderr,
                                                    &format!(
                                                        "clipboard copy failed: {err}"
                                                    ),
                                                    MAX_LOG_LINES,
                                                );
                                                log_selection_state(
                                                    editor,
                                                    &logs,
                                                    "copy failed via system clipboard",
                                                );
                                            }
                                        }
                                    }

                                    // if system clipboard not available or failed, try OSC52
                                    match editor.selection_text() {
                                        Some(text) => match osc52_copy(&text) {
                                            Ok(()) => {
                                                status = String::from("Selection copied via OSC52");
                                                push_log_line(
                                                    &logs,
                                                    LogSource::Stdout,
                                                    &format!(
                                                        "osc52 copy: {}",
                                                        editor.selection_debug_summary()
                                                    ),
                                                    MAX_LOG_LINES,
                                                );
                                            }
                                            Err(err) => {
                                                if !tried_clip {
                                                    push_log_line(
                                                        &logs,
                                                        LogSource::Stderr,
                                                        &format!(
                                                            "clipboard unavailable and osc52 failed: {err}"
                                                        ),
                                                        MAX_LOG_LINES,
                                                    );
                                                    status = String::from(
                                                        "Clipboard and OSC52 copy failed; see logs",
                                                    );
                                                } else {
                                                    push_log_line(
                                                        &logs,
                                                        LogSource::Stderr,
                                                        &format!(
                                                            "OSC52 fallback failed after clipboard attempt: {err}"
                                                        ),
                                                        MAX_LOG_LINES,
                                                    );
                                                    status = String::from(
                                                        "OSC52 copy failed; see logs",
                                                    );
                                                }
                                            }
                                        },
                                        None => {
                                            status = String::from("Select text to copy");
                                        }
                                    }
                                    continue;
                                }
                                if keymap.editor_run_block.matches(&key_event) {
                                    if let Some(line_number) = editor.block_start_for_cursor() {
                                        if run_block_via_session(
                                            editor,
                                            &session_handle,
                                            &logs,
                                            &mut status,
                                            running_line,
                                            line_number,
                                        ) {
                                            continue;
                                        }
                                    } else {
                                        status = String::from(
                                            "Move the cursor inside a runnable block",
                                        );
                                        continue;
                                    }
                                }
                                let action = editor.handle_key(key_event)?;
                                let message = editor.take_status_message();
                                let changed_blocks = editor.take_recently_saved_blocks();
                                if !changed_blocks.is_empty() {
                                    if let Err(err) = session_handle.enqueue_blocks(changed_blocks)
                                    {
                                        push_log_line(
                                            &logs,
                                            LogSource::Stderr,
                                            &format!("queue update failed: {}", err),
                                            MAX_LOG_LINES,
                                        );
                                    }
                                }
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
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture,
        PopKeyboardEnhancementFlags
    )?;
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
    saved_block_hashes: HashMap<usize, u64>,
    recently_saved_blocks: Vec<usize>,
    selection_anchor: Option<TextPosition>,
    selection_head: Option<TextPosition>,
    mouse_selecting: bool,
}

struct LineRender {
    prefix: String,
    content: String,
    prefix_width: usize,
}

struct VisibleLines {
    lines: Vec<LineRender>,
    arrow_width: usize,
    arrow_offset: usize,
}

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug)]
struct TextPosition {
    row: usize,
    col: usize,
}

impl TextPosition {
    fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }
}

#[derive(Default, Clone, Copy)]
struct UiLayout {
    editor_area: Option<Rect>,
    logs_area: Option<Rect>,
}

impl UiLayout {
    fn reset(&mut self) {
        self.editor_area = None;
        self.logs_area = None;
    }

    fn editor_contains(&self, column: u16, row: u16) -> bool {
        self.editor_area
            .map(|rect| rect_contains(rect, column, row))
            .unwrap_or(false)
    }

    fn logs_contains(&self, column: u16, row: u16) -> bool {
        self.logs_area
            .map(|rect| rect_contains(rect, column, row))
            .unwrap_or(false)
    }
}

fn rect_contains(rect: Rect, column: u16, row: u16) -> bool {
    let x = rect.x as u32;
    let y = rect.y as u32;
    let width = rect.width as u32;
    let height = rect.height as u32;
    let col = column as u32;
    let line = row as u32;
    col >= x && col < x + width && line >= y && line < y + height
}

struct LogScrollState {
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
    fn offset(&self) -> usize {
        self.offset
    }

    fn update_metrics(&mut self, total_lines: usize, viewport: usize) {
        self.total_lines = total_lines;
        self.viewport = viewport.max(1);
        if self.follow_tail {
            self.offset = self.max_offset();
        } else {
            self.clamp_to_bounds();
        }
    }

    fn scroll(&mut self, delta: isize) {
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

    fn indicator_text(&self) -> Option<String> {
        if self.total_lines == 0 || self.viewport == 0 {
            return None;
        }
        let start = self.offset + 1;
        let end = (self.offset + self.viewport).min(self.total_lines);
        Some(format!("{start}-{end}/{total}", total = self.total_lines))
    }
}

#[derive(Clone)]
struct CodeBlockMeta {
    fence_line: usize,
    end_line: usize,
    info: String,
}

impl CodeBlockMeta {
    fn is_generated_output(&self) -> bool {
        self.info
            .split_whitespace()
            .any(|token| token.eq_ignore_ascii_case("runbook"))
    }
}

struct EditorRenderInfo {
    inner_x: u16,
    inner_y: u16,
    inner_width: u16,
    inner_height: u16,
    prefix_width: usize,
    arrow_width: usize,
    arrow_offset: usize,
}

impl EditorState {
    fn load(path: PathBuf) -> Result<Self> {
        let (lines, last_disk_mtime) = Self::read_disk_snapshot(&path)?;
        let code_blocks = Self::compute_code_blocks(&lines);
        let saved_block_hashes = Self::compute_block_hashes(&lines, &code_blocks);
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
            saved_block_hashes,
            recently_saved_blocks: Vec::new(),
            selection_anchor: None,
            selection_head: None,
            mouse_selecting: false,
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
        fs::metadata(path)
            .ok()
            .and_then(|meta| meta.modified().ok())
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
        self.saved_block_hashes = Self::compute_block_hashes(&self.lines, &self.code_blocks);
        self.recently_saved_blocks.clear();
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

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn cursor_col(&self) -> usize {
        self.cursor_col
    }

    fn relative_cursor_row(&self) -> usize {
        self.cursor_row.saturating_sub(self.scroll_row)
    }

    fn scroll_row(&self) -> usize {
        self.scroll_row
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

    fn scroll_by(&mut self, delta: isize, viewport_height: usize) {
        if viewport_height == 0 || delta == 0 {
            return;
        }
        let max_scroll = self
            .lines
            .len()
            .saturating_sub(viewport_height.max(1));
        if delta > 0 {
            let delta = delta as usize;
            self.scroll_row = (self.scroll_row + delta).min(max_scroll);
        } else {
            let delta = (-delta) as usize;
            self.scroll_row = self.scroll_row.saturating_sub(delta);
        }
    }

    fn visible_lines_for_render(
        &mut self,
        viewport_height: usize,
        running_block: Option<usize>,
        can_run_blocks: bool,
    ) -> VisibleLines {
        if viewport_height == 0 {
            return VisibleLines {
                lines: Vec::new(),
                arrow_width: RUN_BUTTON_WIDTH,
                arrow_offset: 0,
            };
        }

        self.ensure_code_blocks();

        let total_lines = self.lines.len().max(1);
        let digits_width = digits(total_lines);
        let status_icon_width = STATUS_ICON_READY.chars().count();
        let run_button_width = RUN_BUTTON_WIDTH;
        let run_button_offset = status_icon_width + 1 + digits_width + 1;
        let end = (self.scroll_row + viewport_height).min(self.lines.len());
        let mut rendered = Vec::with_capacity(end.saturating_sub(self.scroll_row));
        for idx in self.scroll_row..end {
            let number = idx + 1;
            let arrow = if self.is_code_block_start(idx) {
                if Some(idx) == running_block {
                    STATUS_ICON_RUNNING
                } else if can_run_blocks {
                    STATUS_ICON_READY
                } else {
                    STATUS_ICON_DISABLED
                }
            } else {
                STATUS_ICON_BLANK
            };
            let run_button = if self.is_code_block_start(idx) {
                if Some(idx) == running_block {
                    RUN_BUTTON_RUNNING
                } else if can_run_blocks {
                    RUN_BUTTON_READY
                } else {
                    RUN_BUTTON_DISABLED
                }
            } else {
                RUN_BUTTON_BLANK
            };
            let prefix = format!(
                "{arrow} {:>width$} {run_button} │ ",
                number,
                width = digits_width
            );
            rendered.push(LineRender {
                prefix: prefix.clone(),
                content: self.lines[idx].clone(),
                prefix_width: prefix.chars().count(),
            });
        }

        VisibleLines {
            lines: rendered,
            arrow_width: run_button_width,
            arrow_offset: run_button_offset,
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

    fn mark_disk_stale(&mut self) {
        if self.dirty {
            return;
        }
        self.last_disk_mtime = None;
    }

    fn save(&mut self) -> Result<()> {
        let contents = self.lines.join("\n");
        fs::write(&self.path, contents)?;
        self.dirty = false;
        self.last_disk_mtime = Self::disk_mtime(&self.path);
        self.ensure_code_blocks();
        let new_hashes = Self::compute_block_hashes(&self.lines, &self.code_blocks);
        let mut changed = Vec::new();
        for (line, hash) in &new_hashes {
            if self.saved_block_hashes.get(line) != Some(hash) {
                changed.push(*line);
            }
        }
        changed.sort_unstable();
        changed.dedup();
        self.recently_saved_blocks = changed;
        self.saved_block_hashes = new_hashes;
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

        let arrow_start = info.arrow_offset;
        let arrow_end = arrow_start + info.arrow_width;
        if rel_x < arrow_start || rel_x >= arrow_end {
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

    fn compute_block_hashes(lines: &[String], blocks: &[CodeBlockMeta]) -> HashMap<usize, u64> {
        let mut hashes = HashMap::new();
        for block in blocks {
            if block.is_generated_output() {
                continue;
            }
            let mut hasher = DefaultHasher::new();
            let mut idx = block.fence_line;
            while idx <= block.end_line && idx < lines.len() {
                lines[idx].hash(&mut hasher);
                '\n'.hash(&mut hasher);
                idx += 1;
            }
            hashes.insert(block.fence_line + 1, hasher.finish());
        }
        hashes
    }

    fn take_recently_saved_blocks(&mut self) -> Vec<usize> {
        std::mem::take(&mut self.recently_saved_blocks)
    }

    fn is_code_block_start(&self, line_idx: usize) -> bool {
        self.code_blocks
            .iter()
            .any(|block| block.fence_line == line_idx && !block.is_generated_output())
    }

    fn has_code_block_at_line(&mut self, line_number: usize) -> bool {
        if line_number == 0 {
            return false;
        }
        self.ensure_code_blocks();
        let idx = line_number.saturating_sub(1);
        self.is_code_block_start(idx)
    }

    fn block_start_for_cursor(&mut self) -> Option<usize> {
        self.ensure_code_blocks();
        let row = self.cursor_row;
        self.code_blocks
            .iter()
            .find(|block| {
                !block.is_generated_output()
                    && row >= block.fence_line
                    && row <= block.end_line
            })
            .map(|block| block.fence_line + 1)
    }

    fn compute_code_blocks(lines: &[String]) -> Vec<CodeBlockMeta> {
        if lines.is_empty() {
            return Vec::new();
        }

        let contents = lines.join("\n");
        let arena = Arena::new();
        let mut options = Options::default();
        options.render.sourcepos = true;
        let root = parse_document(&arena, &contents, &options);

        let mut blocks = Vec::new();
        for node in root.descendants() {
            let data = node.data.borrow();
            if let NodeValue::CodeBlock(cb) = &data.value {
                let start_line = data.sourcepos.start.line.saturating_sub(1);
                if start_line >= lines.len() {
                    continue;
                }
                let end_line = data
                    .sourcepos
                    .end
                    .line
                    .saturating_sub(1)
                    .min(lines.len().saturating_sub(1));
                let info = cb.info.trim().to_string();
                blocks.push(CodeBlockMeta {
                    fence_line: start_line,
                    end_line,
                    info,
                });
            }
        }

        blocks.sort_by_key(|block| block.fence_line);
        blocks
    }

    fn selection_columns_for_line(&self, row: usize) -> Option<(usize, usize)> {
        let (start, end) = self.normalized_selection_range()?;
        if row >= self.lines.len() {
            return None;
        }
        if row < start.row || row > end.row {
            return None;
        }
        let line_len = self.lines.get(row).map(|line| line.len()).unwrap_or(0);
        let mut start_col = if row == start.row { start.col } else { 0 };
        let mut end_col = if row == end.row { end.col } else { line_len };
        start_col = start_col.min(line_len);
        end_col = end_col.min(line_len);
        if start_col == end_col {
            return None;
        }
        if start_col > end_col {
            std::mem::swap(&mut start_col, &mut end_col);
        }
        Some((start_col, end_col))
    }

    fn normalized_selection_range(&self) -> Option<(TextPosition, TextPosition)> {
        let anchor = self.selection_anchor?;
        let head = self.selection_head?;
        if anchor == head {
            return None;
        }
        if anchor <= head {
            Some((anchor, head))
        } else {
            Some((head, anchor))
        }
    }

    fn text_position_from_point(&self, column: u16, row: u16) -> Option<TextPosition> {
        let info = self.render_info.as_ref()?;
        if column < info.inner_x || row < info.inner_y {
            return None;
        }
        let rel_x = (column - info.inner_x) as usize;
        let rel_y = (row - info.inner_y) as usize;
        if rel_x >= info.inner_width as usize || rel_y >= info.inner_height as usize {
            return None;
        }
        if self.lines.is_empty() {
            return None;
        }
        let absolute_row = self.scroll_row + rel_y;
        let row_idx = absolute_row.min(self.lines.len().saturating_sub(1));
        let prefix_width = info.prefix_width;
        let col = if rel_x < prefix_width {
            0
        } else {
            rel_x - prefix_width
        };
        let line_len = self.lines[row_idx].len();
        let col_idx = col.min(line_len);
        Some(TextPosition::new(row_idx, col_idx))
    }

    fn begin_mouse_selection(&mut self, position: TextPosition) {
        let position = self.clamp_text_position(position);
        self.selection_anchor = Some(position);
        self.selection_head = Some(position);
        self.mouse_selecting = true;
        self.set_cursor_position(position);
    }

    fn update_mouse_selection(&mut self, position: TextPosition) {
        if !self.mouse_selecting {
            return;
        }
        let position = self.clamp_text_position(position);
        self.selection_head = Some(position);
        self.set_cursor_position(position);
    }

    fn end_mouse_selection(&mut self) {
        self.mouse_selecting = false;
        if let (Some(anchor), Some(head)) = (self.selection_anchor, self.selection_head) {
            if anchor == head {
                self.clear_selection();
            }
        } else {
            self.clear_selection();
        }
    }

    fn clear_selection(&mut self) {
        self.selection_anchor = None;
        self.selection_head = None;
        self.mouse_selecting = false;
    }

    fn set_cursor_position(&mut self, position: TextPosition) {
        let clamped = self.clamp_text_position(position);
        self.cursor_row = clamped.row;
        self.cursor_col = clamped.col;
    }

    fn clamp_text_position(&self, position: TextPosition) -> TextPosition {
        if self.lines.is_empty() {
            return TextPosition::new(0, 0);
        }
        let row = position.row.min(self.lines.len().saturating_sub(1));
        let line_len = self.lines[row].len();
        let col = position.col.min(line_len);
        TextPosition::new(row, col)
    }

    fn selection_text(&self) -> Option<String> {
        let (start, end) = self.normalized_selection_range()?;
        if start.row >= self.lines.len() {
            return None;
        }
        let mut buffer = String::new();
        let mut wrote_any = false;
        for row in start.row..=end.row {
            let line = self.lines.get(row)?;
            let line_len = line.len();
            let start_idx = if row == start.row {
                start.col.min(line_len)
            } else {
                0
            };
            let end_idx = if row == end.row {
                end.col.min(line_len)
            } else {
                line_len
            };
            if start_idx < end_idx {
                buffer.push_str(&line[start_idx..end_idx]);
                wrote_any = true;
            }
            if row != end.row {
                wrote_any = true;
                buffer.push('\n');
            }
        }
        if wrote_any {
            Some(buffer)
        } else {
            None
        }
    }

    fn copy_selection_to_clipboard(&self, clipboard: &mut Clipboard) -> Result<bool> {
        if let Some(text) = self.selection_text() {
            clipboard
                .set_text(text)
                .map_err(|err| anyhow!("clipboard copy failed: {err}"))?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn selection_debug_summary(&self) -> String {
        if let Some((start, end)) = self.normalized_selection_range() {
            let preview = self.selection_text().unwrap_or_default();
            let preview_snippet: String = preview.chars().take(80).collect();
            format!(
                "start={}:{}, end={}:{}, len={}, preview={:?}",
                start.row,
                start.col,
                end.row,
                end.col,
                preview.len(),
                preview_snippet
            )
        } else {
            "<none>".to_string()
        }
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
        Block::default().borders(Borders::ALL).title("runbook-cli")
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
    can_run_blocks: bool,
}

impl<'a> EditorView<'a> {
    fn new(
        editor: &'a mut EditorState,
        running_block: Option<usize>,
        can_run_blocks: bool,
    ) -> Self {
        Self {
            editor,
            running_block,
            can_run_blocks,
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
        let visible =
            self.editor
                .visible_lines_for_render(viewport, self.running_block, self.can_run_blocks);
        let selection_style = Style::default().fg(SELECTION_FG).bg(SELECTION_BG);
        let mut lines = Vec::with_capacity(visible.lines.len());
        let base_row = self.editor.scroll_row();
        for (idx, line) in visible.lines.iter().enumerate() {
            let mut spans = Vec::new();
            spans.push(Span::styled(
                line.prefix.clone(),
                Style::default().fg(Color::DarkGray),
            ));
            let absolute_row = base_row + idx;
            if let Some((start, end)) = self.editor.selection_columns_for_line(absolute_row) {
                let (before, selected, after) =
                    split_content_segments(&line.content, start, end);
                if !before.is_empty() {
                    spans.push(Span::raw(before));
                }
                if !selected.is_empty() {
                    spans.push(Span::styled(selected, selection_style));
                }
                if !after.is_empty() {
                    spans.push(Span::raw(after));
                }
            } else {
                spans.push(Span::raw(line.content.clone()));
            }
            lines.push(Spans::from(spans));
        }
        frame.render_widget(Paragraph::new(Text::from(lines)), inner);

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
            arrow_width: visible.arrow_width,
            arrow_offset: visible.arrow_offset,
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
    scroll: &'a mut LogScrollState,
}

impl<'a> LogsView<'a> {
    fn new(
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
            self.scroll
                .update_metrics(guard.len(), viewport.max(1));
            if guard.is_empty() {
                Text::from("(no logs yet)")
            } else {
                let start = self.scroll.offset();
                logs_to_text_window(&guard, start, viewport.max(1))
            }
        };
        frame.render_widget(Paragraph::new(text), inner);

        if let Some(label) = self.scroll.indicator_text() {
            if inner.width > 0 {
                let width = inner.width.min(16);
                let x = inner.x + inner.width - width;
                let rect = Rect {
                    x,
                    y: inner.y,
                    width,
                    height: 1,
                };
                let indicator = Paragraph::new(label)
                    .alignment(Alignment::Right)
                    .style(Style::default().fg(Color::DarkGray));
                frame.render_widget(indicator, rect);
            }
        }
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

fn split_content_segments(content: &str, start: usize, end: usize) -> (String, String, String) {
    if content.is_empty() {
        return (String::new(), String::new(), String::new());
    }
    let len = content.len();
    let start_idx = start.min(len);
    let end_idx = end.min(len);
    if start_idx >= end_idx {
        let before = content[..start_idx].to_string();
        let after = content[start_idx..].to_string();
        return (before, String::new(), after);
    }
    let before = content[..start_idx].to_string();
    let selected = content[start_idx..end_idx].to_string();
    let after = content[end_idx..].to_string();
    (before, selected, after)
}

fn scroll_delta_for(kind: MouseEventKind, config: &TuiConfig) -> isize {
    match kind {
        MouseEventKind::ScrollUp => -config.scroll_lines_per_tick,
        MouseEventKind::ScrollDown => config.scroll_lines_per_tick,
        _ => 0,
    }
}

fn osc52_copy(text: &str) -> Result<()> {
    // Limit payload to avoid very large terminal transfers.
    const MAX_OSC52_BYTES: usize = 100 * 1024; // 100 KiB
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
    // OSC52 sequence: ESC ] 52 ; c ; <base64> BEL
    let seq = format!("\x1b]52;c;{}\x07", b64);
    use std::io::Write;
    let mut out = io::stdout();
    out.write_all(seq.as_bytes())?;
    out.flush()?;
    Ok(())
}

fn is_copy_shortcut(event: &KeyEvent) -> bool {
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

fn log_selection_state(editor: &EditorState, logs: &Arc<Mutex<VecDeque<LogRecord>>>, label: &str) {
    let summary = editor.selection_debug_summary();
    push_log_line(
        logs,
        LogSource::Stdout,
        &format!("[SEL] {label}: {summary}"),
        MAX_LOG_LINES,
    );
}

fn log_key_event(logs: &Arc<Mutex<VecDeque<LogRecord>>>, label: &str, event: &KeyEvent) {
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

fn run_block_via_session(
    editor: &mut EditorState,
    session_handle: &SessionHandle,
    logs: &Arc<Mutex<VecDeque<LogRecord>>>,
    status: &mut String,
    running_line: Option<usize>,
    line_number: usize,
) -> bool {
    if line_number == 0 {
        return false;
    }

    if let Some(active) = running_line {
        if active == line_number {
            match session_handle.stop_active(line_number) {
                Ok(()) => {
                    *status = format!("Stop requested for block at line {}", line_number);
                }
                Err(err) => {
                    push_log_line(
                        logs,
                        LogSource::Stderr,
                        &format!("stop request failed at line {}: {}", line_number, err),
                        MAX_LOG_LINES,
                    );
                    *status = String::from("Stop request failed; check logs");
                }
            }
        } else {
            push_log_line(
                logs,
                LogSource::Stdout,
                &format!("run in progress; block {} is active", active),
                MAX_LOG_LINES,
            );
            *status = String::from("Run in progress; wait for current block");
        }
        return true;
    }

    if editor.is_dirty() {
        if let Err(err) = editor.save() {
            push_log_line(
                logs,
                LogSource::Stderr,
                &format!(
                    "save failed before running block at line {}: {}",
                    line_number, err
                ),
                MAX_LOG_LINES,
            );
            *status = format!("Save failed before running block (line {})", line_number);
            return true;
        }
        let saved_blocks = editor.take_recently_saved_blocks();
        let other_blocks: Vec<usize> = saved_blocks
            .into_iter()
            .filter(|line| *line != line_number)
            .collect();
        if !other_blocks.is_empty() {
            if let Err(err) = session_handle.enqueue_blocks(other_blocks) {
                push_log_line(
                    logs,
                    LogSource::Stderr,
                    &format!("queue update failed: {}", err),
                    MAX_LOG_LINES,
                );
            }
        }
        push_log_line(
            logs,
            LogSource::Stdout,
            &format!(
                "saved {} before running block at line {}",
                editor.short_path(),
                line_number
            ),
            MAX_LOG_LINES,
        );
    }

    match session_handle.run_block(line_number) {
        Ok(()) => {
            *status = format!("Starting block at line {}...", line_number);
        }
        Err(err) => {
            push_log_line(
                logs,
                LogSource::Stderr,
                &format!("run-block error at line {}: {}", line_number, err),
                MAX_LOG_LINES,
            );
            *status = format!("Snippet start failed at line {}", line_number);
        }
    }
    true
}

fn digits(mut value: usize) -> usize {
    let mut width = 1;
    while value >= 10 {
        value /= 10;
        width += 1;
    }
    width
}

fn logs_to_text_window(
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
    for record in entries.iter().skip(clamped_start).take(end.saturating_sub(clamped_start)) {
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
