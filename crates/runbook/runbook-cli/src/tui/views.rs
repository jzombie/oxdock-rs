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
