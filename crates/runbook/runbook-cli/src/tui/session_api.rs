use anyhow::Result;

use crate::session::SessionHandle;

pub(crate) trait SessionActions {
    fn run_block(&self, line: usize) -> Result<()>;
    fn stop_active(&self, line: usize) -> Result<()>;
    fn enqueue_blocks(&self, lines: Vec<usize>) -> Result<()>;
    fn shutdown(&self) -> Result<()>;
}

impl SessionActions for SessionHandle {
    fn run_block(&self, line: usize) -> Result<()> {
        SessionHandle::run_block(self, line)
    }

    fn stop_active(&self, line: usize) -> Result<()> {
        SessionHandle::stop_active(self, line)
    }

    fn enqueue_blocks(&self, lines: Vec<usize>) -> Result<()> {
        SessionHandle::enqueue_blocks(self, lines)
    }

    fn shutdown(&self) -> Result<()> {
        SessionHandle::shutdown(self)
    }
}
