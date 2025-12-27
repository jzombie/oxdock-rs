mod run_coordinator;

use self::run_coordinator::{
    FinishDisposition, RunCoordinator, RunDecision, RunToken, StartDisposition, StopCommand,
};
use crate::{
    ExecutionOutputObserver, RunControl, RunnerCache, read_stable_contents, run_block_in_place,
    scan_and_register_runners,
};
use anyhow::{Context, Result};
use oxdock_fs::{GuardedPath, PathResolver, discover_workspace_root};
use std::collections::{HashSet, VecDeque};
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex, mpsc};
use std::thread;
use std::time::Duration;

pub struct SessionHandle {
    commands: mpsc::Sender<SessionCommand>,
}

pub struct SessionEvents {
    receiver: mpsc::Receiver<SessionEvent>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SessionLogSource {
    Stdout,
    Stderr,
}

pub enum SessionCommand {
    RunBlock { line: usize },
    EnqueueBlocks { lines: Vec<usize> },
    StopActive { line: usize },
    Shutdown,
}

pub enum SessionEvent {
    RunStarted {
        line: usize,
    },
    RunCompleted {
        line: usize,
        success: bool,
    },
    RunFailed {
        line: usize,
        error: String,
    },
    Log {
        source: SessionLogSource,
        message: String,
    },
    AutoRunQueued {
        line: usize,
    },
    AutoRunSkipped {
        line: usize,
    },
    Busy {
        requested: usize,
        active: usize,
    },
    StopQueued {
        line: usize,
    },
    StopIssued {
        line: usize,
    },
    StopIgnored {
        line: usize,
    },
}

pub struct SessionConfig {
    pub target: String,
}

pub fn start_session(config: SessionConfig) -> Result<(SessionHandle, SessionEvents)> {
    let workspace_root = discover_workspace_root().context("resolve workspace root")?;
    let resolver = Arc::new(
        PathResolver::new_guarded(workspace_root.clone(), workspace_root.clone())
            .context("create workspace path resolver")?,
    );

    scan_and_register_runners(&workspace_root);

    let process_cwd = std::env::current_dir().context("determine current directory")?;
    let cwd_base = match GuardedPath::new(workspace_root.root(), &process_cwd) {
        Ok(g) => g,
        Err(_) => workspace_root.clone(),
    };

    let watched = resolver
        .resolve_read(&cwd_base, &config.target)
        .with_context(|| format!("resolve markdown path {}", config.target))?;

    let source_dir = watched.parent().unwrap_or_else(|| workspace_root.clone());

    let initial_contents = read_stable_contents(&resolver, &watched)?;

    let cache = Arc::new(Mutex::new(RunnerCache::default()));
    let last_contents = Arc::new(Mutex::new(initial_contents));
    let emit_block_events = Arc::new(AtomicBool::new(false));
    let run_control = Arc::new(RunControl::new());

    let (command_tx, command_rx) = mpsc::channel();
    let (event_tx, event_rx) = mpsc::channel();
    let (completion_tx, completion_rx) = mpsc::channel();

    let state = SessionState {
        resolver,
        workspace_root,
        watched,
        source_dir,
        cache,
        last_contents,
        emit_block_events,
        run_control,
        coordinator: RunCoordinator::new(),
        plan_locked: false,
        deferred_lines: VecDeque::new(),
        deferred_set: HashSet::new(),
        event_tx,
        completion_tx,
        completion_rx,
    };

    thread::Builder::new()
        .name("runbook-run-session".to_string())
        .spawn(move || state.run(command_rx))
        .context("spawn runoff session thread")?;

    Ok((
        SessionHandle {
            commands: command_tx,
        },
        SessionEvents { receiver: event_rx },
    ))
}

impl SessionHandle {
    pub fn run_block(&self, line: usize) -> Result<()> {
        self.commands
            .send(SessionCommand::RunBlock { line })
            .map_err(|err| err.into())
    }

    pub fn enqueue_blocks(&self, lines: Vec<usize>) -> Result<()> {
        self.commands
            .send(SessionCommand::EnqueueBlocks { lines })
            .map_err(|err| err.into())
    }

    pub fn stop_active(&self, line: usize) -> Result<()> {
        self.commands
            .send(SessionCommand::StopActive { line })
            .map_err(|err| err.into())
    }

    pub fn shutdown(&self) -> Result<()> {
        self.commands
            .send(SessionCommand::Shutdown)
            .map_err(|err| err.into())
    }
}

impl SessionEvents {
    pub fn try_recv(&self) -> Option<SessionEvent> {
        self.receiver.try_recv().ok()
    }

    pub fn recv(&self) -> Option<SessionEvent> {
        self.receiver.recv().ok()
    }
}

struct SessionState {
    resolver: Arc<PathResolver>,
    workspace_root: GuardedPath,
    watched: GuardedPath,
    source_dir: GuardedPath,
    cache: Arc<Mutex<RunnerCache>>,
    last_contents: Arc<Mutex<String>>,
    emit_block_events: Arc<AtomicBool>,
    run_control: Arc<RunControl>,
    coordinator: RunCoordinator,
    plan_locked: bool,
    deferred_lines: VecDeque<usize>,
    deferred_set: HashSet<usize>,
    event_tx: mpsc::Sender<SessionEvent>,
    completion_tx: mpsc::Sender<RunCompletion>,
    completion_rx: mpsc::Receiver<RunCompletion>,
}

impl SessionState {
    fn lock_plan(&mut self) {
        if !self.plan_locked {
            self.plan_locked = true;
        }
    }

    fn finish_plan_cycle(&mut self) {
        self.plan_locked = false;
        let mut scheduled = self.coordinator.has_queued();
        if !self.deferred_lines.is_empty() {
            let pending: Vec<usize> = self.deferred_lines.drain(..).collect();
            self.deferred_set.clear();
            let added = self.coordinator.enqueue_lines(&pending);
            if !added.is_empty() {
                scheduled = true;
            }
        } else {
            self.deferred_set.clear();
        }
        if scheduled && !self.coordinator.has_active() {
            self.start_next_queued();
        }
    }

    fn run(mut self, commands: mpsc::Receiver<SessionCommand>) {
        let mut shutdown_requested = false;
        loop {
            if shutdown_requested
                && !self.coordinator.has_active()
                && self.coordinator.queued_lines().is_empty()
            {
                break;
            }

            while let Ok(completion) = self.completion_rx.try_recv() {
                self.handle_completion(completion);
            }

            let timeout = if self.coordinator.has_active() {
                Duration::from_millis(60)
            } else {
                Duration::from_millis(200)
            };

            match commands.recv_timeout(timeout) {
                Ok(command) => {
                    if self.handle_command(command) {
                        shutdown_requested = true;
                    }
                }
                Err(mpsc::RecvTimeoutError::Timeout) => {}
                Err(mpsc::RecvTimeoutError::Disconnected) => {
                    shutdown_requested = true;
                }
            }

            while let Ok(completion) = self.completion_rx.try_recv() {
                self.handle_completion(completion);
            }

            if !self.coordinator.has_active() {
                self.start_next_queued();
            }
        }
    }

    fn handle_command(&mut self, command: SessionCommand) -> bool {
        match command {
            SessionCommand::RunBlock { line } => {
                if !self.plan_locked {
                    self.coordinator.remove_from_queue(line);
                }
                match self.coordinator.request_run(line) {
                    RunDecision::Dispatch { line, token } => {
                        self.start_run(line, token);
                    }
                    RunDecision::Busy { current_line } => {
                        let _ = self.event_tx.send(SessionEvent::Busy {
                            requested: line,
                            active: current_line,
                        });
                    }
                }
            }
            SessionCommand::EnqueueBlocks { lines } => {
                if self.plan_locked {
                    let mut added = Vec::new();
                    for &line in &lines {
                        if line == 0 {
                            continue;
                        }
                        if self.coordinator.contains(line) || self.deferred_set.contains(&line) {
                            continue;
                        }
                        if self.deferred_set.insert(line) {
                            self.deferred_lines.push_back(line);
                            added.push(line);
                        }
                    }
                    let added_set: HashSet<usize> = added.iter().copied().collect();
                    for line in added {
                        let _ = self.event_tx.send(SessionEvent::AutoRunQueued { line });
                    }
                    for &line in &lines {
                        if !added_set.contains(&line) {
                            let _ = self.event_tx.send(SessionEvent::AutoRunSkipped { line });
                        }
                    }
                } else {
                    let added = self.coordinator.enqueue_lines(&lines);
                    let added_set: HashSet<usize> = added.iter().copied().collect();
                    for line in added {
                        let _ = self.event_tx.send(SessionEvent::AutoRunQueued { line });
                    }
                    for line in lines {
                        if !added_set.contains(&line) {
                            let _ = self.event_tx.send(SessionEvent::AutoRunSkipped { line });
                        }
                    }
                    if !self.coordinator.has_active() {
                        self.start_next_queued();
                    }
                }
            }
            SessionCommand::StopActive { line } => {
                self.handle_stop_request(line);
            }
            SessionCommand::Shutdown => {
                if self.run_control.request_cancel() {
                    let _ = self.event_tx.send(SessionEvent::StopIssued {
                        line: self.coordinator.active_line().unwrap_or(0),
                    });
                }
                return true;
            }
        }
        false
    }

    fn handle_stop_request(&mut self, line: usize) {
        match self.coordinator.request_stop(line) {
            StopCommand::NoActiveRun => {
                let _ = self.event_tx.send(SessionEvent::StopIgnored { line });
            }
            StopCommand::WrongRun { active_line } => {
                let _ = self.event_tx.send(SessionEvent::Busy {
                    requested: line,
                    active: active_line,
                });
            }
            StopCommand::AlreadyStopping => {
                let _ = self.event_tx.send(SessionEvent::StopIgnored { line });
            }
            StopCommand::QueueUntilStart => {
                let _ = self.event_tx.send(SessionEvent::StopQueued { line });
            }
            StopCommand::SendNow { token } => {
                if self.run_control.request_cancel() {
                    self.coordinator.confirm_stop_sent(token);
                    let _ = self.event_tx.send(SessionEvent::StopIssued { line });
                } else {
                    self.coordinator.stop_failed(token);
                    let _ = self.event_tx.send(SessionEvent::StopIgnored { line });
                }
            }
        }
    }

    fn start_next_queued(&mut self) {
        if let Some(request) = self.coordinator.prepare_next_queued() {
            self.start_run(request.line, request.token);
        }
    }

    fn start_run(&mut self, line: usize, token: RunToken) {
        self.lock_plan();
        let observer = Arc::new(SessionOutputObserver::new(self.event_tx.clone()));
        let observer_for_run: Arc<dyn ExecutionOutputObserver> = observer.clone();
        let resolver = Arc::clone(&self.resolver);
        let workspace_root = self.workspace_root.clone();
        let watched = self.watched.clone();
        let source_dir = self.source_dir.clone();
        let cache = Arc::clone(&self.cache);
        let last_contents = Arc::clone(&self.last_contents);
        let emit_block_events = Arc::clone(&self.emit_block_events);
        let run_control = Arc::clone(&self.run_control);
        let completion_tx = self.completion_tx.clone();

        thread::spawn(move || {
            let guard = run_control.begin();
            let cancel_token = run_control.token().clone();
            let result = {
                let mut cache_guard = cache.lock().unwrap();
                let mut last_guard = last_contents.lock().unwrap();
                run_block_in_place(
                    &resolver,
                    &workspace_root,
                    &watched,
                    &source_dir,
                    &mut cache_guard,
                    &mut last_guard,
                    line,
                    emit_block_events,
                    Some(&cancel_token),
                    Some(observer_for_run),
                )
            };
            drop(guard);
            observer.flush();
            let completion = match result {
                Ok(Some(exec)) => RunCompletion::Success {
                    line,
                    token,
                    success: exec.success,
                },
                Ok(None) => RunCompletion::NoBlock { line, token },
                Err(err) => RunCompletion::Error {
                    line,
                    token,
                    error: err.to_string(),
                },
            };
            let _ = completion_tx.send(completion);
        });

        match self.coordinator.start_result(line) {
            StartDisposition::Expected { send_stop } => {
                let _ = self.event_tx.send(SessionEvent::RunStarted { line });
                if let Some(token) = send_stop {
                    if self.run_control.request_cancel() {
                        self.coordinator.confirm_stop_sent(token);
                        let _ = self.event_tx.send(SessionEvent::StopIssued { line });
                    } else {
                        self.coordinator.stop_failed(token);
                    }
                }
            }
            StartDisposition::Unexpected { active_line } => {
                let _ = self.event_tx.send(SessionEvent::RunFailed {
                    line,
                    error: match active_line {
                        Some(other) => format!(
                            "coordinator desync: expected line {} but active {}",
                            line, other
                        ),
                        None => "coordinator unexpectedly idle".to_string(),
                    },
                });
                self.coordinator.cancel_active(token);
            }
        }
    }

    fn handle_completion(&mut self, completion: RunCompletion) {
        match completion {
            RunCompletion::Success {
                line,
                token,
                success,
            } => {
                self.finish_run(line, token, success, None);
            }
            RunCompletion::NoBlock { line, token } => {
                self.finish_run(
                    line,
                    token,
                    false,
                    Some(format!("No code block covering line {}", line)),
                );
            }
            RunCompletion::Error { line, token, error } => {
                self.finish_run(line, token, false, Some(error));
            }
        }
    }

    fn finish_run(&mut self, line: usize, _token: RunToken, success: bool, error: Option<String>) {
        match self.coordinator.finish_result(line) {
            FinishDisposition::Expected { has_more_queued } => {
                if let Some(message) = error {
                    let _ = self.event_tx.send(SessionEvent::RunFailed {
                        line,
                        error: message,
                    });
                } else {
                    let _ = self
                        .event_tx
                        .send(SessionEvent::RunCompleted { line, success });
                }
                if has_more_queued {
                    self.start_next_queued();
                } else {
                    self.finish_plan_cycle();
                }
            }
            FinishDisposition::Unexpected { active_line } => {
                let err = match active_line {
                    Some(expected) => format!(
                        "coordinator mismatch: finished line {} but expected {}",
                        line, expected
                    ),
                    None => format!("coordinator idle while finishing line {}", line),
                };
                let _ = self
                    .event_tx
                    .send(SessionEvent::RunFailed { line, error: err });
                self.coordinator.clear_active();
                self.finish_plan_cycle();
            }
        }
    }
}

enum RunCompletion {
    Success {
        line: usize,
        token: RunToken,
        success: bool,
    },
    NoBlock {
        line: usize,
        token: RunToken,
    },
    Error {
        line: usize,
        token: RunToken,
        error: String,
    },
}

struct SessionOutputObserver {
    tx: mpsc::Sender<SessionEvent>,
    stdout_buf: Mutex<Vec<u8>>,
    stderr_buf: Mutex<Vec<u8>>,
}

impl SessionOutputObserver {
    fn new(tx: mpsc::Sender<SessionEvent>) -> Self {
        Self {
            tx,
            stdout_buf: Mutex::new(Vec::new()),
            stderr_buf: Mutex::new(Vec::new()),
        }
    }

    fn push(&self, chunk: &[u8], source: SessionLogSource) {
        let buffer = match source {
            SessionLogSource::Stdout => &self.stdout_buf,
            SessionLogSource::Stderr => &self.stderr_buf,
        };
        let mut guard = buffer.lock().unwrap();
        guard.extend_from_slice(chunk);
        while let Some(pos) = guard.iter().position(|&b| b == b'\n') {
            let line = guard.drain(..=pos).collect::<Vec<u8>>();
            let message = String::from_utf8_lossy(&line).to_string();
            let _ = self.tx.send(SessionEvent::Log { source, message });
        }
    }

    fn flush(&self) {
        self.flush_stream(SessionLogSource::Stdout);
        self.flush_stream(SessionLogSource::Stderr);
    }

    fn flush_stream(&self, source: SessionLogSource) {
        let buffer = match source {
            SessionLogSource::Stdout => &self.stdout_buf,
            SessionLogSource::Stderr => &self.stderr_buf,
        };
        let mut guard = buffer.lock().unwrap();
        if !guard.is_empty() {
            let message = String::from_utf8_lossy(&guard).to_string();
            guard.clear();
            let _ = self.tx.send(SessionEvent::Log { source, message });
        }
    }
}

impl ExecutionOutputObserver for SessionOutputObserver {
    fn on_stdout(&self, chunk: &[u8]) {
        self.push(chunk, SessionLogSource::Stdout);
    }

    fn on_stderr(&self, chunk: &[u8]) {
        self.push(chunk, SessionLogSource::Stderr);
    }
}

#[cfg(test)]
mod tests {
    use super::{ExecutionOutputObserver, SessionEvent, SessionLogSource, SessionOutputObserver};
    use std::sync::mpsc;

    #[test]
    fn observer_splits_lines_and_flushes() {
        let (tx, rx) = mpsc::channel();
        let observer = SessionOutputObserver::new(tx);

        observer.on_stdout(b"hello\nworld");
        let event = rx.recv().expect("log event");
        match event {
            SessionEvent::Log { source, message } => {
                assert_eq!(source, SessionLogSource::Stdout);
                assert_eq!(message, "hello\n");
            }
            _ => panic!("unexpected event"),
        }
        assert!(matches!(rx.try_recv(), Err(mpsc::TryRecvError::Empty)));

        observer.flush();
        let event = rx.recv().expect("flush log event");
        match event {
            SessionEvent::Log { source, message } => {
                assert_eq!(source, SessionLogSource::Stdout);
                assert_eq!(message, "world");
            }
            _ => panic!("unexpected event"),
        }
    }

    #[test]
    fn observer_tracks_stderr_and_flush() {
        let (tx, rx) = mpsc::channel();
        let observer = SessionOutputObserver::new(tx);

        observer.on_stderr(b"warn 1\nwarn 2\n");
        for expected in ["warn 1\n", "warn 2\n"] {
            let event = rx.recv().expect("stderr log");
            match event {
                SessionEvent::Log { source, message } => {
                    assert_eq!(source, SessionLogSource::Stderr);
                    assert_eq!(message, expected);
                }
                _ => panic!("unexpected event"),
            }
        }

        observer.flush();
        assert!(matches!(rx.try_recv(), Err(mpsc::TryRecvError::Empty)));
    }
}
