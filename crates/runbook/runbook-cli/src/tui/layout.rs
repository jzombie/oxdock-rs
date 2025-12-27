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
