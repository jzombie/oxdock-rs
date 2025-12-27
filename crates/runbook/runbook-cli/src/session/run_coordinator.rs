use std::collections::{HashSet, VecDeque};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RunToken(u64);

impl RunToken {
    fn new(value: u64) -> Self {
        RunToken(value)
    }
}

enum ActiveState {
    AwaitingStart,
    Started,
}

struct ActiveRun {
    line: usize,
    token: RunToken,
    state: ActiveState,
    stop_requested: bool,
    stop_sent: bool,
}

impl ActiveRun {
    fn new(line: usize, token: RunToken) -> Self {
        Self {
            line,
            token,
            state: ActiveState::AwaitingStart,
            stop_requested: false,
            stop_sent: false,
        }
    }
}

pub struct RunCoordinator {
    queue: VecDeque<usize>,
    queued: HashSet<usize>,
    current: Option<ActiveRun>,
    next_token: u64,
}

pub struct RunRequest {
    pub line: usize,
    pub token: RunToken,
}

pub enum RunDecision {
    Dispatch { line: usize, token: RunToken },
    Busy { current_line: usize },
}

pub enum StopCommand {
    NoActiveRun,
    WrongRun { active_line: usize },
    AlreadyStopping,
    QueueUntilStart,
    SendNow { token: RunToken },
}

pub enum StartDisposition {
    Expected { send_stop: Option<RunToken> },
    Unexpected { active_line: Option<usize> },
}

pub enum FinishDisposition {
    Expected { has_more_queued: bool },
    Unexpected { active_line: Option<usize> },
}

impl RunCoordinator {
    pub fn new() -> Self {
        Self {
            queue: VecDeque::new(),
            queued: HashSet::new(),
            current: None,
            next_token: 0,
        }
    }

    pub fn active_line(&self) -> Option<usize> {
        self.current.as_ref().map(|active| active.line)
    }

    pub fn has_active(&self) -> bool {
        self.current.is_some()
    }

    pub fn enqueue_lines(&mut self, lines: &[usize]) -> Vec<usize> {
        let mut added = Vec::new();
        for &line in lines {
            if line == 0 {
                continue;
            }
            if self
                .current
                .as_ref()
                .map(|active| active.line == line)
                .unwrap_or(false)
            {
                continue;
            }
            if self.queued.insert(line) {
                self.queue.push_back(line);
                added.push(line);
            }
        }
        added
    }

    pub fn contains(&self, line: usize) -> bool {
        if self
            .current
            .as_ref()
            .map(|active| active.line == line)
            .unwrap_or(false)
        {
            return true;
        }
        self.queued.contains(&line)
    }

    pub fn has_queued(&self) -> bool {
        !self.queue.is_empty()
    }

    pub fn remove_from_queue(&mut self, line: usize) {
        if self.queued.remove(&line) {
            self.queue.retain(|&value| value != line);
        }
    }

    pub fn queued_lines(&self) -> Vec<usize> {
        self.queue.iter().copied().collect()
    }

    pub fn request_run(&mut self, line: usize) -> RunDecision {
        if let Some(active) = &self.current {
            return RunDecision::Busy {
                current_line: active.line,
            };
        }
        let token = self.next_token();
        self.current = Some(ActiveRun::new(line, token));
        RunDecision::Dispatch { line, token }
    }

    pub fn cancel_active(&mut self, token: RunToken) {
        if matches!(
            self.current.as_ref().map(|active| active.token == token),
            Some(true)
        ) {
            self.current = None;
        }
    }

    pub fn request_stop(&mut self, line: usize) -> StopCommand {
        let Some(active) = self.current.as_mut() else {
            return StopCommand::NoActiveRun;
        };
        if active.line != line {
            return StopCommand::WrongRun {
                active_line: active.line,
            };
        }
        if active.stop_sent {
            return StopCommand::AlreadyStopping;
        }
        active.stop_requested = true;
        match active.state {
            ActiveState::Started => StopCommand::SendNow {
                token: active.token,
            },
            ActiveState::AwaitingStart => StopCommand::QueueUntilStart,
        }
    }

    pub fn confirm_stop_sent(&mut self, token: RunToken) {
        if let Some(active) = self.current.as_mut()
            && active.token == token
        {
            active.stop_sent = true;
        }
    }

    pub fn stop_failed(&mut self, token: RunToken) {
        if let Some(active) = self.current.as_mut()
            && active.token == token
        {
            active.stop_sent = false;
        }
    }

    pub fn start_result(&mut self, line: usize) -> StartDisposition {
        let Some(active) = self.current.as_mut() else {
            return StartDisposition::Unexpected { active_line: None };
        };
        if active.line != line {
            return StartDisposition::Unexpected {
                active_line: Some(active.line),
            };
        }
        active.state = ActiveState::Started;
        if active.stop_requested && !active.stop_sent {
            active.stop_sent = true;
            StartDisposition::Expected {
                send_stop: Some(active.token),
            }
        } else {
            StartDisposition::Expected { send_stop: None }
        }
    }

    pub fn finish_result(&mut self, line: usize) -> FinishDisposition {
        let Some(active) = self.current.as_ref() else {
            return FinishDisposition::Unexpected { active_line: None };
        };
        if active.line != line {
            return FinishDisposition::Unexpected {
                active_line: Some(active.line),
            };
        }
        self.current = None;
        FinishDisposition::Expected {
            has_more_queued: !self.queue.is_empty(),
        }
    }

    pub fn prepare_next_queued(&mut self) -> Option<RunRequest> {
        if self.current.is_some() {
            return None;
        }
        self.prepare_next_internal()
    }

    pub fn clear_active(&mut self) {
        self.current = None;
    }

    fn prepare_next_internal(&mut self) -> Option<RunRequest> {
        let line = self.queue.pop_front()?;
        self.queued.remove(&line);
        let token = self.next_token();
        self.current = Some(ActiveRun::new(line, token));
        Some(RunRequest { line, token })
    }

    fn next_token(&mut self) -> RunToken {
        let token = RunToken::new(self.next_token);
        self.next_token = self.next_token.wrapping_add(1);
        token
    }
}
