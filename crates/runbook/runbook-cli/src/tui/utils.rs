use crossterm::event::MouseEventKind;

use super::config::TuiConfig;

pub(crate) fn scroll_delta_for(kind: MouseEventKind, config: &TuiConfig) -> isize {
    match kind {
        MouseEventKind::ScrollUp => -config.scroll_lines_per_tick,
        MouseEventKind::ScrollDown => config.scroll_lines_per_tick,
        _ => 0,
    }
}
