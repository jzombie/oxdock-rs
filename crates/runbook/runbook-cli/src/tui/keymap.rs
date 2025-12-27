use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

#[derive(Clone, Copy)]
pub(crate) struct KeyBinding {
    code: KeyCode,
    modifiers: KeyModifiers,
}

impl KeyBinding {
    pub(crate) const fn new(code: KeyCode, modifiers: KeyModifiers) -> Self {
        Self { code, modifiers }
    }

    pub(crate) fn matches(&self, event: &KeyEvent) -> bool {
        event.code == self.code && event.modifiers == self.modifiers
    }
}

#[derive(Clone, Copy)]
pub(crate) struct KeyBindings {
    pub dashboard_quit: KeyBinding,
    pub dashboard_open_editor: KeyBinding,
    pub dashboard_ready: KeyBinding,
    pub editor_run_block: KeyBinding,
}

impl KeyBindings {
    pub(crate) fn default() -> Self {
        let none = KeyModifiers::empty();
        Self {
            dashboard_quit: KeyBinding::new(KeyCode::Char('q'), none),
            dashboard_open_editor: KeyBinding::new(KeyCode::Char('e'), none),
            dashboard_ready: KeyBinding::new(KeyCode::Char('r'), none),
            editor_run_block: KeyBinding::new(KeyCode::Char('r'), KeyModifiers::CONTROL),
        }
    }
}
