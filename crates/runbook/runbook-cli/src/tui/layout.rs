use ratatui::layout::Rect;

#[derive(Default, Clone, Copy)]
pub(crate) struct UiLayout {
    pub editor_area: Option<Rect>,
    pub logs_area: Option<Rect>,
}

impl UiLayout {
    pub(crate) fn reset(&mut self) {
        self.editor_area = None;
        self.logs_area = None;
    }

    pub(crate) fn editor_contains(&self, column: u16, row: u16) -> bool {
        self.editor_area
            .map(|rect| rect_contains(rect, column, row))
            .unwrap_or(false)
    }

    pub(crate) fn logs_contains(&self, column: u16, row: u16) -> bool {
        self.logs_area
            .map(|rect| rect_contains(rect, column, row))
            .unwrap_or(false)
    }
}

pub(crate) fn rect_contains(rect: Rect, column: u16, row: u16) -> bool {
    let x = rect.x as u32;
    let y = rect.y as u32;
    let width = rect.width as u32;
    let height = rect.height as u32;
    let col = column as u32;
    let line = row as u32;
    col >= x && col < x + width && line >= y && line < y + height
}

#[cfg(test)]
mod tests {
    use super::{UiLayout, rect_contains};
    use ratatui::layout::Rect;

    #[test]
    fn rect_contains_bounds() {
        let rect = Rect::new(2, 3, 4, 5);
        assert!(rect_contains(rect, 2, 3));
        assert!(rect_contains(rect, 5, 7));
        assert!(!rect_contains(rect, 6, 7));
        assert!(!rect_contains(rect, 5, 8));
    }

    #[test]
    fn layout_tracks_editor_and_logs() {
        let mut layout = UiLayout::default();
        let editor = Rect::new(0, 0, 10, 2);
        let logs = Rect::new(0, 3, 10, 2);
        layout.editor_area = Some(editor);
        layout.logs_area = Some(logs);

        assert!(layout.editor_contains(1, 1));
        assert!(!layout.editor_contains(1, 4));
        assert!(layout.logs_contains(1, 4));
        assert!(!layout.logs_contains(1, 1));

        layout.reset();
        assert!(!layout.editor_contains(1, 1));
        assert!(!layout.logs_contains(1, 4));
    }
}
