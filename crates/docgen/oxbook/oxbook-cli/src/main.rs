use anyhow::{Context, Result};
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};
use std::thread;

fn main() -> Result<()> {
    let mut use_tui = false;
    let mut cli_args = Vec::new();
    for arg in std::env::args().skip(1) {
        match arg.as_str() {
            "--tui" | "-t" => use_tui = true,
            "tui" if !use_tui && cli_args.is_empty() => use_tui = true,
            _ => cli_args.push(arg),
        }
    }

    if use_tui {
        return run_tui(cli_args);
    }

    oxbook_cli::run()
}

fn run_tui(cli_args: Vec<String>) -> Result<()> {
    use std::{io, time::Duration};

    use crossterm::{
        event::{self, Event as CEvent, KeyCode},
        execute,
        terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
    };
    use ratatui::{
        Terminal,
        backend::CrosstermBackend,
        layout::{Alignment, Constraint, Direction, Layout},
        style::{Modifier, Style},
        widgets::{Block, Borders, Paragraph},
    };

    const MAX_LOG_LINES: usize = 800;

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.clear()?;

    let result = (|| -> Result<()> {
        let logs: Arc<Mutex<VecDeque<String>>> = Arc::new(Mutex::new(VecDeque::new()));
        let mut status = String::from("Idle");
        let mut child: Option<std::process::Child> = None;

        loop {
            if let Some(proc) = child.as_mut() {
                if let Some(exit) = proc.try_wait()? {
                    {
                        let mut guard = logs.lock().unwrap();
                        guard.push_back(format!("process exited: {}\n", exit));
                        trim_logs(&mut guard, MAX_LOG_LINES);
                    }
                    child = None;
                    status = String::from("Idle");
                }
            }

            terminal.draw(|f| {
                let size = f.size();
                let chunks = Layout::default()
                    .direction(Direction::Vertical)
                    .margin(2)
                    .constraints([
                        Constraint::Length(3),
                        Constraint::Min(1),
                        Constraint::Length(3),
                    ])
                    .split(size);

                let header = Paragraph::new("Oxbook TUI — press 'r' to run, 'q' to quit")
                    .block(Block::default().borders(Borders::ALL).title("oxbook-cli"))
                    .alignment(Alignment::Center);
                f.render_widget(header, chunks[0]);

                let mid = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([Constraint::Min(3), Constraint::Length(12)])
                    .split(chunks[1]);

                let target_info = if cli_args.is_empty() {
                    "No path provided (defaults to README.md)".to_string()
                } else {
                    format!("Target: {}", cli_args.join(" "))
                };
                let body = Paragraph::new(format!(
                    "(Minimal IDE prototype)\n\n{target_info}\nPress 'r' to run oxbook-cli within this session."
                ))
                .block(Block::default().borders(Borders::ALL).title("Controls"));
                f.render_widget(body, mid[0]);

                let log_snapshot = {
                    let guard = logs.lock().unwrap();
                    if guard.is_empty() {
                        String::from("(no logs yet)")
                    } else {
                        let mut buf = String::new();
                        for entry in guard.iter() {
                            buf.push_str(entry);
                        }
                        buf
                    }
                };
                let logs_widget = Paragraph::new(log_snapshot)
                    .block(Block::default().borders(Borders::ALL).title("Logs"));
                f.render_widget(logs_widget, mid[1]);

                let footer = Paragraph::new(status.clone())
                    .block(Block::default().borders(Borders::ALL).title("Status"))
                    .style(Style::default().add_modifier(Modifier::BOLD));
                f.render_widget(footer, chunks[2]);
            })?;

            if event::poll(Duration::from_millis(120))? {
                if let CEvent::Key(key) = event::read()? {
                    match key.code {
                        KeyCode::Char('q') => {
                            if let Some(mut proc) = child.take() {
                                let _ = proc.kill();
                                let _ = proc.wait();
                            }
                            break;
                        }
                        KeyCode::Char('r') => {
                            if child.is_some() {
                                status = String::from("Already running");
                                continue;
                            }
                            match spawn_cli_child(&cli_args, logs.clone(), MAX_LOG_LINES) {
                                Ok(new_child) => {
                                    status = String::from("Running...");
                                    child = Some(new_child);
                                }
                                Err(err) => {
                                    let mut guard = logs.lock().unwrap();
                                    guard.push_back(format!("spawn error: {err}\n"));
                                    trim_logs(&mut guard, MAX_LOG_LINES);
                                    status = String::from("Spawn failed");
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        Ok(())
    })();

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;
    result
}

fn trim_logs(logs: &mut VecDeque<String>, max: usize) {
    while logs.len() > max {
        logs.pop_front();
    }
}

fn spawn_cli_child(
    cli_args: &[String],
    logs: Arc<Mutex<VecDeque<String>>>,
    max_logs: usize,
) -> Result<std::process::Child> {
    use std::io::{BufRead, BufReader};

    let exe = std::env::current_exe().context("locate current executable")?;
    let mut cmd = std::process::Command::new(exe);
    if !cli_args.is_empty() {
        cmd.args(cli_args);
    }
    cmd.stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());

    let mut child = cmd.spawn().context("spawn oxbook-cli child")?;

    if let Some(stdout) = child.stdout.take() {
        let stdout_logs = logs.clone();
        thread::spawn(move || {
            let mut reader = BufReader::new(stdout);
            let mut line = String::new();
            while reader.read_line(&mut line).unwrap_or(0) > 0 {
                let mut guard = stdout_logs.lock().unwrap();
                guard.push_back(line.clone());
                trim_logs(&mut guard, max_logs);
                line.clear();
            }
        });
    }

    if let Some(stderr) = child.stderr.take() {
        let stderr_logs = logs.clone();
        thread::spawn(move || {
            let mut reader = BufReader::new(stderr);
            let mut line = String::new();
            while reader.read_line(&mut line).unwrap_or(0) > 0 {
                let mut guard = stderr_logs.lock().unwrap();
                guard.push_back(line.clone());
                trim_logs(&mut guard, max_logs);
                line.clear();
            }
        });
    }

    Ok(child)
}
