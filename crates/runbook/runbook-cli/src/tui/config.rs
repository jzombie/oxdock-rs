use ratatui::style::Color;

pub(crate) const MAX_LOG_LINES: usize = 800;
pub(crate) const DEFAULT_SCROLL_LINES_PER_TICK: isize = 6;
pub(crate) const SELECTION_FG: Color = Color::Black;
pub(crate) const SELECTION_BG: Color = Color::LightCyan;

#[derive(Debug, Clone)]
pub(crate) struct TuiConfig {
    pub scroll_lines_per_tick: isize,
}

impl Default for TuiConfig {
    fn default() -> Self {
        Self {
            scroll_lines_per_tick: DEFAULT_SCROLL_LINES_PER_TICK,
        }
    }
}
