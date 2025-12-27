use crossterm::event::MouseEventKind;

use super::config::TuiConfig;

pub(crate) fn scroll_delta_for(kind: MouseEventKind, config: &TuiConfig) -> isize {
    match kind {
        MouseEventKind::ScrollUp => -config.scroll_lines_per_tick,
        MouseEventKind::ScrollDown => config.scroll_lines_per_tick,
        _ => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::{TuiConfig, scroll_delta_for};
    use crossterm::event::MouseEventKind;

    #[test]
    fn scroll_delta_for_mouse_wheel() {
        let config = TuiConfig {
            scroll_lines_per_tick: 4,
        };
        assert_eq!(scroll_delta_for(MouseEventKind::ScrollUp, &config), -4);
        assert_eq!(scroll_delta_for(MouseEventKind::ScrollDown, &config), 4);
        assert_eq!(scroll_delta_for(MouseEventKind::Moved, &config), 0);
    }
}
