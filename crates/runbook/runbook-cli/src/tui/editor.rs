use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use std::time::SystemTime;

use anyhow::{Result, anyhow};
use arboard::Clipboard;
use comrak::{Arena, Options, nodes::NodeValue, parse_document};
use line_ending::LineEnding;
use oxdock_fs::{GuardedPath, PathResolver, normalized_path, to_forward_slashes};
use ratatui::Frame;
use ratatui::backend::Backend;
use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
use ratatui::text::{Span, Spans, Text};
use ratatui::widgets::Paragraph;

use super::config::{SELECTION_BG, SELECTION_FG};
use super::views::FramedView;

const STATUS_ICON_READY: &str = "○";
const STATUS_ICON_RUNNING: &str = "●";
const STATUS_ICON_DISABLED: &str = "◌";
const STATUS_ICON_BLANK: &str = " ";
const RUN_BUTTON_READY: &str = "▶";
const RUN_BUTTON_RUNNING: &str = "■";
const RUN_BUTTON_DISABLED: &str = "·";
const RUN_BUTTON_BLANK: &str = " ";
const RUN_BUTTON_WIDTH: usize = 1;

pub(crate) enum EditorAction {
    Continue,
    Exit,
}

pub(crate) struct EditorState {
    path: GuardedPath,
    resolver: Arc<PathResolver>,
    pub lines: Vec<String>,
    cursor_row: usize,
    cursor_col: usize,
    scroll_row: usize,
    dirty: bool,
    last_disk_mtime: Option<SystemTime>,
    pending_status: Option<String>,
    code_blocks: Vec<CodeBlockMeta>,
    code_blocks_dirty: bool,
    pub render_info: Option<EditorRenderInfo>,
    saved_block_hashes: HashMap<usize, u64>,
    recently_saved_blocks: Vec<usize>,
    pub selection_anchor: Option<TextPosition>,
    pub selection_head: Option<TextPosition>,
    pub mouse_selecting: bool,
    pub drag_scroll: isize,
}

pub(crate) struct EditorView<'a> {
    pub editor: &'a mut EditorState,
    pub running_block: Option<usize>,
    pub can_run_blocks: bool,
}

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub(crate) struct TextPosition {
    pub row: usize,
    pub col: usize,
}

#[derive(Clone)]
pub(crate) struct CodeBlockMeta {
    pub fence_line: usize,
    pub end_line: usize,
    pub info: String,
}

#[derive(Clone, Copy)]
pub(crate) struct EditorRenderInfo {
    pub inner_x: u16,
    pub inner_y: u16,
    pub inner_width: u16,
    pub inner_height: u16,
    pub prefix_width: usize,
    pub arrow_width: usize,
    pub arrow_offset: usize,
    pub copy_button: Option<Rect>,
}

struct LineRender {
    prefix: String,
    content: String,
    prefix_width: usize,
}

pub(crate) struct VisibleLines {
    lines: Vec<LineRender>,
    arrow_width: usize,
    arrow_offset: usize,
}

impl<'a> EditorView<'a> {
    pub(crate) fn new(
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
    fn block<'b>(&'b self) -> ratatui::widgets::Block<'b> {
        ratatui::widgets::Block::default()
            .borders(ratatui::widgets::Borders::ALL)
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
                let (before, selected, after) = split_content_segments(&line.content, start, end);
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

        let mut copy_button_rect: Option<Rect> = None;
        if let Some((_start, end)) = self.editor.normalized_selection_range()
            && !self.editor.mouse_selecting
            && end.row >= self.editor.scroll_row
        {
            let rel_row = end.row - self.editor.scroll_row;
            if let Some(line) = visible.lines.get(rel_row) {
                let prefix = line.prefix_width.min(inner.width as usize) as u16;
                let content_col = end.col as u16;
                let button_text = "[Copy]";
                let button_width = button_text.chars().count() as u16 + 1;
                let mut x = inner.x + prefix + content_col;
                if x + button_width > inner.x + inner.width {
                    if inner.x + inner.width > button_width {
                        x = inner.x + inner.width - button_width;
                    } else {
                        x = inner.x;
                    }
                }
                let y = inner.y + rel_row as u16;
                copy_button_rect = Some(Rect {
                    x,
                    y,
                    width: button_width,
                    height: 1,
                });
            }
        }
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
            copy_button: copy_button_rect,
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

        if let Some(info) = self.editor.render_info.as_ref()
            && let Some(rect) = info.copy_button
        {
            let txt = Paragraph::new("[Copy]")
                .style(Style::default().fg(Color::Black).bg(Color::White))
                .alignment(ratatui::layout::Alignment::Center);
            frame.render_widget(txt, rect);
        }
    }
}

impl EditorState {
    pub(crate) fn load(path: GuardedPath, resolver: Arc<PathResolver>) -> Result<Self> {
        let (lines, last_disk_mtime) = Self::read_disk_snapshot(&resolver, &path)?;
        let code_blocks = Self::compute_code_blocks(&lines);
        let saved_block_hashes = Self::compute_block_hashes(&lines, &code_blocks);
        Ok(Self {
            path,
            resolver,
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
            drag_scroll: 0,
        })
    }

    pub(crate) fn read_disk_snapshot(
        resolver: &PathResolver,
        path: &GuardedPath,
    ) -> Result<(Vec<String>, Option<SystemTime>)> {
        let contents = match resolver.read_to_string(path) {
            Ok(buf) => buf,
            Err(err) => {
                if Self::is_not_found(&err) {
                    String::new()
                } else {
                    return Err(err);
                }
            }
        };
        let lines = Self::split_into_lines(contents);
        let modified = Self::disk_mtime(resolver, path);
        Ok((lines, modified))
    }

    pub(crate) fn sync_with_disk(&mut self) -> Result<Option<String>> {
        if self.dirty {
            return Ok(None);
        }

        let modified = Self::disk_mtime(&self.resolver, &self.path);
        if modified == self.last_disk_mtime {
            return Ok(None);
        }

        let contents = match self.resolver.read_to_string(&self.path) {
            Ok(buf) => buf,
            Err(err) => {
                if Self::is_not_found(&err) {
                    String::new()
                } else {
                    return Err(err);
                }
            }
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

    fn is_not_found(err: &anyhow::Error) -> bool {
        err.downcast_ref::<std::io::Error>()
            .map(|io_err| io_err.kind() == std::io::ErrorKind::NotFound)
            .unwrap_or(false)
    }

    pub(crate) fn take_status_message(&mut self) -> Option<String> {
        self.pending_status.take()
    }

    pub(crate) fn mark_disk_stale(&mut self) {
        if self.dirty {
            return;
        }
        self.last_disk_mtime = None;
    }

    pub(crate) fn save(&mut self) -> Result<()> {
        let contents = self.lines.join("\n");
        self.resolver.write_file(&self.path, contents.as_bytes())?;
        self.dirty = false;
        self.last_disk_mtime = Self::disk_mtime(&self.resolver, &self.path);
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

    pub(crate) fn handle_key(&mut self, key: crossterm::event::KeyEvent) -> Result<EditorAction> {
        use crossterm::event::{KeyCode, KeyModifiers};
        if key.modifiers.contains(KeyModifiers::CONTROL)
            && let KeyCode::Char(ch) = key.code
            && matches!(ch, 's' | 'S')
        {
            self.save()?;
            self.pending_status = Some(format!("Saved {}", self.short_path()));
            return Ok(EditorAction::Continue);
        }
        if let Some((_start, _end)) = self.normalized_selection_range() {
            match key.code {
                KeyCode::Backspace => {
                    self.delete_selection();
                    return Ok(EditorAction::Continue);
                }
                KeyCode::Delete => {
                    self.delete_selection();
                    return Ok(EditorAction::Continue);
                }
                _ => {}
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

    pub(crate) fn take_recently_saved_blocks(&mut self) -> Vec<usize> {
        std::mem::take(&mut self.recently_saved_blocks)
    }

    pub(crate) fn is_dirty(&self) -> bool {
        self.dirty
    }

    pub(crate) fn block_start_for_cursor(&mut self) -> Option<usize> {
        self.ensure_code_blocks();
        let row = self.cursor_row;
        self.code_blocks
            .iter()
            .find(|block| {
                !block.is_generated_output() && row >= block.fence_line && row <= block.end_line
            })
            .map(|block| block.fence_line + 1)
    }

    pub(crate) fn short_path(&self) -> String {
        let path = self.path.as_path();

        if let Some(workspace_root) = self.resolver.workspace_root()
            && let Ok(stripped) = path.strip_prefix(workspace_root.as_path())
        {
            let rel = stripped.to_string_lossy();
            return to_forward_slashes(rel.as_ref());
        }

        if let Ok(stripped) = path.strip_prefix(self.resolver.root().as_path()) {
            let rel = stripped.to_string_lossy();
            return to_forward_slashes(rel.as_ref());
        }

        normalized_path(&self.path)
    }

    pub(crate) fn cursor_col(&self) -> usize {
        self.cursor_col
    }

    pub(crate) fn relative_cursor_row(&self) -> usize {
        self.cursor_row.saturating_sub(self.scroll_row)
    }

    pub(crate) fn scroll_row(&self) -> usize {
        self.scroll_row
    }

    pub(crate) fn adjust_scroll(&mut self, viewport_height: usize) {
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

    pub(crate) fn scroll_by(&mut self, delta: isize, viewport_height: usize) {
        if viewport_height == 0 || delta == 0 {
            return;
        }
        let max_scroll = self.lines.len().saturating_sub(viewport_height.max(1));
        if delta > 0 {
            let delta = delta as usize;
            self.scroll_row = (self.scroll_row + delta).min(max_scroll);
        } else {
            let delta = (-delta) as usize;
            self.scroll_row = self.scroll_row.saturating_sub(delta);
        }
    }

    pub(crate) fn visible_lines_for_render(
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

    pub(crate) fn hit_test_run_glyph(&self, column: u16, row: u16) -> Option<usize> {
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

    pub(crate) fn text_position_from_point(&self, column: u16, row: u16) -> Option<TextPosition> {
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
        let col = rel_x.saturating_sub(prefix_width);
        let line_len = self.lines[row_idx].len();
        let col_idx = col.min(line_len);
        Some(TextPosition::new(row_idx, col_idx))
    }

    pub(crate) fn begin_mouse_selection(&mut self, position: TextPosition) {
        let position = self.clamp_text_position(position);
        self.selection_anchor = Some(position);
        self.selection_head = Some(position);
        self.mouse_selecting = true;
        self.set_cursor_position(position);
    }

    pub(crate) fn update_mouse_selection(&mut self, position: TextPosition) {
        if !self.mouse_selecting {
            return;
        }
        let position = self.clamp_text_position(position);
        self.selection_head = Some(position);
        self.set_cursor_position(position);
    }

    pub(crate) fn end_mouse_selection(&mut self) {
        self.drag_scroll = 0;
        self.mouse_selecting = false;
        if let (Some(anchor), Some(head)) = (self.selection_anchor, self.selection_head) {
            if anchor == head {
                self.clear_selection();
            }
        } else {
            self.clear_selection();
        }
    }

    pub(crate) fn clear_selection(&mut self) {
        self.selection_anchor = None;
        self.selection_head = None;
        self.mouse_selecting = false;
        self.drag_scroll = 0;
    }

    pub(crate) fn selection_text(&self) -> Option<String> {
        let (start, end) = self.normalized_selection_range()?;
        if start.row >= self.lines.len() {
            return None;
        }
        let eol = LineEnding::from_current_platform().denormalize("\n");
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
                buffer.push_str(&eol);
            }
        }
        if wrote_any { Some(buffer) } else { None }
    }

    pub(crate) fn copy_selection_to_clipboard(&self, clipboard: &mut Clipboard) -> Result<bool> {
        if let Some(text) = self.selection_text() {
            clipboard
                .set_text(text)
                .map_err(|err| anyhow!("clipboard copy failed: {err}"))?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub(crate) fn selection_debug_summary(&self) -> String {
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

    pub(crate) fn delete_selection(&mut self) {
        let (start, end) = match self.normalized_selection_range() {
            Some(range) => range,
            None => return,
        };

        if start.row == end.row {
            if let Some(line) = self.lines.get_mut(start.row) {
                let s = start.col.min(line.len());
                let e = end.col.min(line.len());
                if s < e {
                    line.replace_range(s..e, "");
                }
            }
            self.cursor_row = start.row;
            self.cursor_col = start.col.min(self.current_line_len());
        } else {
            if let Some(start_line) = self.lines.get_mut(start.row) {
                let s = start.col.min(start_line.len());
                start_line.truncate(s);
            }
            let mut end_remainder = String::new();
            if let Some(end_line) = self.lines.get(end.row) {
                let e = end.col.min(end_line.len());
                end_remainder = end_line[e..].to_string();
            }
            let remove_from = start.row + 1;
            let remove_to = end.row;
            if remove_to >= remove_from && remove_to < self.lines.len() {
                self.lines.drain(remove_from..=remove_to);
            }
            if let Some(start_line) = self.lines.get_mut(start.row) {
                start_line.push_str(&end_remainder);
            }
            self.cursor_row = start.row;
            self.cursor_col = start.col.min(self.current_line_len());
        }

        self.clear_selection();
        self.dirty = true;
        self.invalidate_layout();
    }

    pub(crate) fn normalized_selection_range(&self) -> Option<(TextPosition, TextPosition)> {
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

    pub(crate) fn selection_columns_for_line(&self, row: usize) -> Option<(usize, usize)> {
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

    pub(crate) fn hit_test_copy_button(&self, column: u16, row: u16) -> bool {
        self.render_info
            .as_ref()
            .and_then(|info| info.copy_button)
            .map(|rect| super::layout::rect_contains(rect, column, row))
            .unwrap_or(false)
    }

    pub(crate) fn set_cursor_position(&mut self, position: TextPosition) {
        let clamped = self.clamp_text_position(position);
        self.cursor_row = clamped.row;
        self.cursor_col = clamped.col;
    }

    pub(crate) fn insert_text_at_cursor(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }
        let lines: Vec<&str> = text.split('\n').collect();
        if lines.len() == 1 {
            let line = lines[0];
            if let Some(current) = self.lines.get_mut(self.cursor_row) {
                current.insert_str(self.cursor_col, line);
                self.cursor_col += line.len();
            }
        } else {
            let first = lines.first().copied().unwrap_or("");
            let last = lines.last().copied().unwrap_or("");
            if let Some(current) = self.lines.get_mut(self.cursor_row) {
                let trailing = current.split_off(self.cursor_col);
                current.push_str(first);
                self.cursor_col = current.len();
                for mid in &lines[1..lines.len() - 1] {
                    self.cursor_row += 1;
                    self.lines.insert(self.cursor_row, mid.to_string());
                }
                self.cursor_row += 1;
                let mut new_line = String::from(last);
                new_line.push_str(&trailing);
                self.lines.insert(self.cursor_row, new_line);
                self.cursor_col = last.len();
            }
        }
        self.dirty = true;
        self.invalidate_layout();
    }

    pub(crate) fn update_drag_selection_edge(&mut self, viewport: usize, upwards: bool) {
        if upwards {
            let head_row = self.scroll_row;
            let col = self
                .cursor_col
                .min(self.lines.get(head_row).map(|l| l.len()).unwrap_or(0));
            self.update_mouse_selection(TextPosition::new(head_row, col));
        } else {
            let head_row = self.scroll_row.saturating_add(viewport.saturating_sub(1));
            let head_row = head_row.min(self.lines.len().saturating_sub(1));
            let col = self
                .cursor_col
                .min(self.lines.get(head_row).map(|l| l.len()).unwrap_or(0));
            self.update_mouse_selection(TextPosition::new(head_row, col));
        }
    }
}

impl TextPosition {
    pub(crate) fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }
}

impl CodeBlockMeta {
    fn is_generated_output(&self) -> bool {
        self.info
            .split_whitespace()
            .any(|token| token.eq_ignore_ascii_case("runbook"))
    }
}

impl EditorState {
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

    fn disk_mtime(resolver: &PathResolver, path: &GuardedPath) -> Option<SystemTime> {
        resolver
            .metadata(path)
            .ok()
            .and_then(|meta| meta.modified().ok())
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
        if let Some(line) = self.lines.get_mut(self.cursor_row)
            && self.cursor_col < line.len()
        {
            line.remove(self.cursor_col);
            self.dirty = true;
            self.invalidate_layout();
            return;
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
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
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

    fn is_code_block_start(&self, line_idx: usize) -> bool {
        self.code_blocks
            .iter()
            .any(|block| block.fence_line == line_idx && !block.is_generated_output())
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

    fn clamp_text_position(&self, position: TextPosition) -> TextPosition {
        if self.lines.is_empty() {
            return TextPosition::new(0, 0);
        }
        let row = position.row.min(self.lines.len().saturating_sub(1));
        let line_len = self.lines[row].len();
        let col = position.col.min(line_len);
        TextPosition::new(row, col)
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

fn digits(mut value: usize) -> usize {
    let mut width = 1;
    while value >= 10 {
        value /= 10;
        width += 1;
    }
    width
}

#[cfg(test)]
mod tests {
    use super::{
        EditorRenderInfo, EditorState, TextPosition, VisibleLines, digits, split_content_segments,
    };
    use oxdock_fs::{GuardedPath, PathResolver};
    use std::sync::Arc;

    fn make_editor(lines: &[&str]) -> EditorState {
        let temp = GuardedPath::tempdir().expect("tempdir");
        let root = temp.as_guarded_path().clone();
        let resolver =
            Arc::new(PathResolver::new_guarded(root.clone(), root.clone()).expect("resolver"));
        let path = root.join("doc.md").expect("doc path");
        let mut line_vec: Vec<String> = lines.iter().map(|line| line.to_string()).collect();
        if line_vec.is_empty() {
            line_vec.push(String::new());
        }
        let code_blocks = EditorState::compute_code_blocks(&line_vec);
        let saved_block_hashes = EditorState::compute_block_hashes(&line_vec, &code_blocks);
        EditorState {
            path,
            resolver,
            lines: line_vec,
            cursor_row: 0,
            cursor_col: 0,
            scroll_row: 0,
            dirty: false,
            last_disk_mtime: None,
            pending_status: None,
            code_blocks,
            code_blocks_dirty: false,
            render_info: None,
            saved_block_hashes,
            recently_saved_blocks: Vec::new(),
            selection_anchor: None,
            selection_head: None,
            mouse_selecting: false,
            drag_scroll: 0,
        }
    }

    #[test]
    fn split_content_segments_handles_ranges() {
        let (before, selected, after) = split_content_segments("hello", 1, 4);
        assert_eq!(before, "h");
        assert_eq!(selected, "ell");
        assert_eq!(after, "o");

        let (before, selected, after) = split_content_segments("hi", 3, 5);
        assert_eq!(before, "hi");
        assert_eq!(selected, "");
        assert_eq!(after, "");
    }

    #[test]
    fn digits_counts_width() {
        assert_eq!(digits(0), 1);
        assert_eq!(digits(9), 1);
        assert_eq!(digits(10), 2);
        assert_eq!(digits(999), 3);
    }

    #[test]
    fn selection_text_and_delete_across_lines() {
        let mut editor = make_editor(&["hello", "world"]);
        editor.begin_mouse_selection(TextPosition::new(0, 2));
        editor.update_mouse_selection(TextPosition::new(1, 1));
        let eol = line_ending::LineEnding::from_current_platform().denormalize("\n");
        let expected = format!("llo{eol}w");
        assert_eq!(editor.selection_text().as_deref(), Some(expected.as_str()));

        editor.delete_selection();
        assert_eq!(editor.lines, vec![String::from("heorld")]);
        assert_eq!(editor.cursor_col(), 2);
        assert!(!editor.mouse_selecting);
    }

    #[test]
    fn insert_text_at_cursor_handles_multiline() {
        let mut editor = make_editor(&["hi"]);
        editor.set_cursor_position(TextPosition::new(0, 1));
        editor.insert_text_at_cursor("a\nb\nc");
        assert_eq!(
            editor.lines,
            vec![String::from("ha"), String::from("b"), String::from("ci")]
        );
        assert_eq!(editor.cursor_row, 2);
        assert_eq!(editor.cursor_col, 1);
    }

    fn render_and_store(editor: &mut EditorState, visible: &VisibleLines) {
        editor.render_info = Some(EditorRenderInfo {
            inner_x: 0,
            inner_y: 0,
            inner_width: 20,
            inner_height: 4,
            prefix_width: visible
                .lines
                .first()
                .map(|line| line.prefix_width)
                .unwrap_or(0),
            arrow_width: visible.arrow_width,
            arrow_offset: visible.arrow_offset,
            copy_button: None,
        });
    }

    #[test]
    fn visible_lines_include_run_glyphs() {
        let mut editor = make_editor(&["```sh", "echo", "```", "after"]);
        let visible = editor.visible_lines_for_render(4, None, true);
        let prefix = &visible.lines[0].prefix;
        assert!(prefix.contains(super::STATUS_ICON_READY));
        assert!(prefix.contains(super::RUN_BUTTON_READY));
    }

    #[test]
    fn hit_test_run_glyph_and_text_position() {
        let mut editor = make_editor(&["```sh", "echo", "```", "after"]);
        let visible = editor.visible_lines_for_render(4, None, true);
        render_and_store(&mut editor, &visible);

        let col = visible.arrow_offset as u16;
        assert_eq!(editor.hit_test_run_glyph(col, 0), Some(0));
        assert_eq!(editor.hit_test_run_glyph(0, 3), None);

        let pos = editor.text_position_from_point(
            (visible.lines[0].prefix_width + 2) as u16,
            0,
        );
        assert_eq!(pos, Some(TextPosition::new(0, 2)));
    }
}
