use anyhow::{Result, bail};
use anyhow::Context as _;
use oxdock_fs::GuardedPath;
use oxdock_process::{BackgroundHandle, ChildHandle, CommandBuilder, shell_program};
use std::thread;
use std::time::{Duration, Instant};

#[test]
fn child_process_can_be_stopped_before_completion() -> Result<()> {
    let temp = GuardedPath::tempdir().context("create temp dir")?;
    let root = temp.as_guarded_path().clone();
    let started = root.join("started.txt")?;
    let done = root.join("done.txt")?;

    let mut builder = CommandBuilder::new(shell_program());
    #[cfg(windows)]
    {
        builder.arg("/C");
    }
    #[cfg(not(windows))]
    {
        builder.arg("-c");
    }
    builder.arg(long_running_script());
    builder.current_dir(root.as_path());

    let mut child = ChildGuard::new(builder.spawn().context("spawn shell script")?);

    wait_for_path(&started, Duration::from_secs(5))
        .context("start marker was never written before attempting to stop the process")?;

    {
        let mut handle = child.take();
        handle.kill().context("kill child process tree")?;
        let status = handle.wait().context("wait for killed child to exit")?;
        assert!(
            !status.success(),
            "killed process should not report a successful status"
        );
    }

    assert!(
        started.exists(),
        "start marker should exist after killing the process"
    );
    assert!(
        !done.exists(),
        "done marker should not exist because the process was stopped early"
    );

    Ok(())
}

fn wait_for_path(path: &GuardedPath, timeout: Duration) -> Result<()> {
    let start = Instant::now();
    while start.elapsed() < timeout {
        if path.exists() {
            return Ok(());
        }
        thread::sleep(Duration::from_millis(25));
    }
    bail!("timed out waiting for {}", path);
}

#[cfg(windows)]
fn long_running_script() -> &'static str {
    "echo started> started.txt && ping -n 6 127.0.0.1 > NUL && echo done> done.txt"
}

#[cfg(not(windows))]
fn long_running_script() -> &'static str {
    "printf start > started.txt; sleep 5; printf done > done.txt"
}

/// Ensures the spawned child process tree is terminated on panic paths.
struct ChildGuard(Option<ChildHandle>);

impl ChildGuard {
    fn new(handle: ChildHandle) -> Self {
        Self(Some(handle))
    }

    fn take(&mut self) -> ChildHandle {
        self.0.take().expect("child handle already taken")
    }
}

impl Drop for ChildGuard {
    fn drop(&mut self) {
        if let Some(handle) = self.0.as_mut() {
            let _ = handle.kill();
            let _ = handle.wait();
        }
    }
}
