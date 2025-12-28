use ratatui::Frame;
use ratatui::backend::Backend;
use ratatui::layout::{Alignment, Rect};
use ratatui::style::{Modifier, Style};
use ratatui::widgets::{Block, Borders, Paragraph};

pub(crate) trait FramedView {
    fn block<'a>(&'a self) -> Block<'a>;
    fn render_inner<B: Backend>(&mut self, frame: &mut Frame<'_, B>, inner: Rect);

    fn render<B: Backend>(&mut self, frame: &mut Frame<'_, B>, area: Rect) {
        let block = self.block();
        let inner = block.inner(area);
        frame.render_widget(block, area);
        self.render_inner(frame, inner);
    }
}

pub(crate) struct HeaderView<'a> {
    text: &'a str,
}

impl<'a> HeaderView<'a> {
    pub(crate) fn new(text: &'a str) -> Self {
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

pub(crate) struct ControlsView {
    body: String,
}

impl ControlsView {
    pub(crate) fn new(body: String) -> Self {
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

pub(crate) struct StatusView {
    message: String,
}

impl StatusView {
    pub(crate) fn new(message: String) -> Self {
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

#[cfg(test)]
mod tests {
    use super::{ControlsView, FramedView, HeaderView, StatusView};
    use ratatui::Terminal;
    use ratatui::backend::TestBackend;
    use ratatui::layout::Rect;

    fn buffer_text(terminal: &Terminal<TestBackend>) -> String {
        let buffer = terminal.backend().buffer();
        let mut out = String::new();
        for y in 0..buffer.area.height {
            for x in 0..buffer.area.width {
                out.push_str(&buffer.get(x, y).symbol);
            }
            out.push('\n');
        }
        out
    }

    #[test]
    fn header_view_renders_text() {
        let backend = TestBackend::new(30, 3);
        let mut terminal = Terminal::new(backend).expect("terminal");
        terminal
            .draw(|f| {
                let mut view = HeaderView::new("Hello");
                view.render(f, Rect::new(0, 0, 30, 3));
            })
            .expect("draw");
        let text = buffer_text(&terminal);
        assert!(text.contains("Hello"));
        assert!(text.contains("runbook-cli"));
    }

    #[test]
    fn controls_view_renders_body() {
        let backend = TestBackend::new(30, 5);
        let mut terminal = Terminal::new(backend).expect("terminal");
        terminal
            .draw(|f| {
                let mut view = ControlsView::new("Use keys".to_string());
                view.render(f, Rect::new(0, 0, 30, 5));
            })
            .expect("draw");
        let text = buffer_text(&terminal);
        assert!(text.contains("Use keys"));
        assert!(text.contains("Controls"));
    }

    #[test]
    fn status_view_renders_message() {
        let backend = TestBackend::new(30, 3);
        let mut terminal = Terminal::new(backend).expect("terminal");
        terminal
            .draw(|f| {
                let mut view = StatusView::new("Running".to_string());
                view.render(f, Rect::new(0, 0, 30, 3));
            })
            .expect("draw");
        let text = buffer_text(&terminal);
        assert!(text.contains("Running"));
        assert!(text.contains("Status"));
    }
}
