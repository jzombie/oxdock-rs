use std::collections::VecDeque;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use anyhow::{Context, Result};
use arboard::Clipboard;
use crossterm::{
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event as CEvent, KeyEvent, KeyEventKind,
        MouseButton, MouseEventKind, PopKeyboardEnhancementFlags, PushKeyboardEnhancementFlags,
    },
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use ratatui::Terminal;
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout};

use crate::session::{SessionConfig, SessionEvent, SessionHandle, SessionLogSource, start_session};

use super::clipboard::{
    is_copy_shortcut, is_paste_shortcut, log_key_event, log_selection_state, run_copy_action,
};
use super::config::{MAX_LOG_LINES, TuiConfig};
use super::editor::{EditorAction, EditorState, EditorView};
use super::keymap::KeyBindings;
use super::layout::UiLayout;
use super::logs::{LogRecord, LogScrollState, LogSource, LogsView, push_log_line, trim_logs};
use super::utils::scroll_delta_for;
use super::views::{ControlsView, FramedView, HeaderView, StatusView};
use oxdock_fs::{GuardedPath, PathResolver, discover_workspace_root};

pub fn run_tui(cli_args: Vec<String>) -> Result<()> {
    let config = TuiConfig::default();
    let workspace_root = discover_workspace_root().context("resolve workspace root")?;
    let resolver = Arc::new(
        PathResolver::new_guarded(workspace_root.clone(), workspace_root.clone())
            .context("workspace resolver")?,
    );
    let target_path = resolve_target(&cli_args, &resolver)?;
    let session_target = cli_args
        .first()
        .cloned()
        .unwrap_or_else(|| String::from("README.md"));

    enable_raw_mode()?;
    let mut stdout = std::io::stdout();
    let keyboard_flags = crossterm::event::KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES
        | crossterm::event::KeyboardEnhancementFlags::REPORT_ALL_KEYS_AS_ESCAPE_CODES
        | crossterm::event::KeyboardEnhancementFlags::REPORT_ALTERNATE_KEYS
        | crossterm::event::KeyboardEnhancementFlags::REPORT_EVENT_TYPES;
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
                        if let Some(editor) = mode.editor_mut() {
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
                        if let Some(editor) = mode.editor_mut() {
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

            if let Some(editor) = mode.editor_mut()
                && let Some(message) = editor.sync_with_disk()?
            {
                status = message;
            }

            if let Some(editor) = mode.editor_mut()
                && editor.mouse_selecting
                && editor.drag_scroll != 0
            {
                let viewport = editor
                    .render_info
                    .as_ref()
                    .map(|i| i.inner_height as usize)
                    .or_else(|| ui_layout.editor_area.map(|r| r.height as usize))
                    .unwrap_or(1);
                editor.scroll_by(editor.drag_scroll, viewport);
                if editor.drag_scroll < 0 {
                    editor.update_drag_selection_edge(viewport, true);
                } else {
                    editor.update_drag_selection_edge(viewport, false);
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
                            EditorView::new(editor.as_mut(), running_line_idx, can_run_blocks);
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
                        ) && ui_layout.logs_contains(mouse_event.column, mouse_event.row)
                        {
                            let delta = scroll_delta_for(mouse_event.kind, &config);
                            log_scroll_state.scroll(delta);
                            handled = true;
                        }
                        if handled {
                            continue;
                        }
                        if let Some(editor) = mode.editor_mut() {
                            match mouse_event.kind {
                                MouseEventKind::ScrollUp | MouseEventKind::ScrollDown => {
                                    if ui_layout
                                        .editor_contains(mouse_event.column, mouse_event.row)
                                        && let Some(info) = editor.render_info.as_ref()
                                    {
                                        let viewport = info.inner_height as usize;
                                        let delta = scroll_delta_for(mouse_event.kind, &config);
                                        editor.scroll_by(delta, viewport);
                                    }
                                }
                                MouseEventKind::Down(MouseButton::Left) => {
                                    if editor
                                        .hit_test_copy_button(mouse_event.column, mouse_event.row)
                                    {
                                        let s = run_copy_action(editor, &logs, &mut clipboard);
                                        status = s;
                                        continue;
                                    }
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

                                    if let Some(position) = editor.text_position_from_point(
                                        mouse_event.column,
                                        mouse_event.row,
                                    ) {
                                        editor.clear_selection();
                                        editor.drag_scroll = 0;
                                        editor.begin_mouse_selection(position);
                                        log_selection_state(editor, &logs, "mouse-down selection");
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
                                        if let Some(position) = editor.text_position_from_point(
                                            mouse_event.column,
                                            mouse_event.row,
                                        ) {
                                            editor.update_mouse_selection(position);
                                            editor.drag_scroll = 0;
                                            log_selection_state(
                                                editor,
                                                &logs,
                                                "mouse-drag selection",
                                            );
                                        } else if let Some(info) = editor.render_info.as_ref() {
                                            let viewport = info.inner_height as usize;
                                            if mouse_event.row < info.inner_y {
                                                editor.drag_scroll = -1;
                                                editor.update_drag_selection_edge(viewport, true);
                                                log_selection_state(
                                                    editor,
                                                    &logs,
                                                    "mouse-drag enter auto-scroll up",
                                                );
                                            } else if mouse_event.row
                                                >= info.inner_y + info.inner_height
                                            {
                                                editor.drag_scroll = 1;
                                                editor.update_drag_selection_edge(viewport, false);
                                                log_selection_state(
                                                    editor,
                                                    &logs,
                                                    "mouse-drag enter auto-scroll down",
                                                );
                                            }
                                        }
                                    }
                                }
                                MouseEventKind::Up(MouseButton::Left) => {
                                    if editor.mouse_selecting {
                                        editor.end_mouse_selection();
                                        log_selection_state(editor, &logs, "mouse-up selection");
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
                                if handle_dashboard_keys(
                                    key_event,
                                    &mut status,
                                    &mut mode,
                                    &keymap,
                                    &session_handle,
                                    &logs,
                                    &target_path,
                                    &resolver,
                                ) {
                                    break Ok(());
                                }
                            }
                            UiMode::Editor(editor) => {
                                let editor = editor.as_mut();
                                if handle_editor_shortcuts(
                                    key_event,
                                    editor,
                                    &mut clipboard,
                                    &logs,
                                    &mut status,
                                    &session_handle,
                                    &keymap,
                                    running_line,
                                )? {
                                    continue;
                                }

                                let action = editor.handle_key(key_event)?;
                                let message = editor.take_status_message();
                                let changed_blocks = editor.take_recently_saved_blocks();
                                if !changed_blocks.is_empty()
                                    && let Err(err) =
                                        session_handle.enqueue_blocks(changed_blocks)
                                {
                                    push_log_line(
                                        &logs,
                                        LogSource::Stderr,
                                        &format!("queue update failed: {}", err),
                                        MAX_LOG_LINES,
                                    );
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

fn resolve_target(cli_args: &[String], resolver: &PathResolver) -> Result<GuardedPath> {
    let raw = cli_args
        .first()
        .map(|s| s.as_str())
        .unwrap_or("README.md");
    resolver
        .parse_env_path(resolver.root(), raw)
        .with_context(|| format!("resolve editor target {raw}"))
}

#[allow(clippy::too_many_arguments)]
fn handle_dashboard_keys(
    key_event: KeyEvent,
    status: &mut String,
    mode: &mut UiMode,
    keymap: &KeyBindings,
    session_handle: &SessionHandle,
    logs: &Arc<Mutex<VecDeque<LogRecord>>>,
    target_path: &GuardedPath,
    resolver: &Arc<PathResolver>,
) -> bool {
    if keymap.dashboard_quit.matches(&key_event) {
        let _ = session_handle.shutdown();
        return true;
    } else if keymap.dashboard_open_editor.matches(&key_event) {
        match EditorState::load(target_path.clone(), Arc::clone(resolver)) {
            Ok(editor) => {
                *status = format!("Editing {}", editor.short_path());
                *mode = UiMode::Editor(Box::new(editor));
            }
            Err(err) => {
                push_log_line(
                    logs,
                    LogSource::Stderr,
                    &format!("editor load error: {err}"),
                    MAX_LOG_LINES,
                );
                *status = String::from("Editor failed");
            }
        }
    } else if keymap.dashboard_ready.matches(&key_event) {
        *status = String::from("Session ready; open the editor to run blocks");
    }
    false
}

#[allow(clippy::too_many_arguments)]
fn handle_editor_shortcuts(
    key_event: KeyEvent,
    editor: &mut EditorState,
    clipboard: &mut Option<Clipboard>,
    logs: &Arc<Mutex<VecDeque<LogRecord>>>,
    status: &mut String,
    session_handle: &SessionHandle,
    keymap: &KeyBindings,
    running_line: Option<usize>,
) -> Result<bool> {
    if is_paste_shortcut(&key_event) {
        log_key_event(logs, "paste shortcut", &key_event);
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
                    *status = String::from("Clipboard unavailable in this terminal");
                }
            }
        }
        if let Some(clip) = clipboard.as_mut() {
            match clip.get_text() {
                Ok(text) => {
                    if editor.normalized_selection_range().is_some() {
                        editor.delete_selection();
                    }
                    editor.insert_text_at_cursor(&text);
                    *status = String::from("Pasted from clipboard");
                }
                Err(err) => {
                    push_log_line(
                        logs,
                        LogSource::Stderr,
                        &format!("clipboard paste failed: {err}"),
                        MAX_LOG_LINES,
                    );
                    *status = String::from("Clipboard paste failed; see logs");
                }
            }
        }
    }
    if is_copy_shortcut(&key_event) {
        log_key_event(logs, "copy shortcut", &key_event);
        log_selection_state(editor, logs, "copy shortcut start");
        *status = run_copy_action(editor, logs, clipboard);
        return Ok(true);
    }

    if keymap.editor_run_block.matches(&key_event) {
        if let Some(line_number) = editor.block_start_for_cursor() {
            if run_block_via_session(
                editor,
                session_handle,
                logs,
                status,
                running_line,
                line_number,
            ) {
                return Ok(true);
            }
        } else {
            *status = String::from("Move the cursor inside a runnable block");
            return Ok(true);
        }
    }
    Ok(false)
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
        if !other_blocks.is_empty()
            && let Err(err) = session_handle.enqueue_blocks(other_blocks)
        {
            push_log_line(
                logs,
                LogSource::Stderr,
                &format!("queue update failed: {}", err),
                MAX_LOG_LINES,
            );
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

enum UiMode {
    Dashboard,
    Editor(Box<EditorState>),
}

impl UiMode {
    fn editor_mut(&mut self) -> Option<&mut EditorState> {
        match self {
            UiMode::Dashboard => None,
            UiMode::Editor(editor) => Some(editor.as_mut()),
        }
    }
}
