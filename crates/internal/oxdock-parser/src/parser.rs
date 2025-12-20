use crate::ast::{Guard, Step, StepKind};
use crate::lexer::{self, GuardToken, LexedCommand, Token};
use anyhow::{Result, anyhow, bail};
use std::collections::VecDeque;

#[derive(Clone)]
struct ScopeFrame {
    line_no: usize,
    had_command: bool,
}

pub struct ScriptParser {
    tokens: VecDeque<Token>,
    steps: Vec<Step>,
    guard_stack: Vec<Vec<Vec<Guard>>>,
    pending_guards: Option<Vec<Vec<Guard>>>,
    pending_inline_guards: Option<Vec<Vec<Guard>>>,
    pending_can_open_block: bool,
    pending_scope_enters: usize,
    scope_stack: Vec<ScopeFrame>,
}

impl ScriptParser {
    pub fn new(input: &str) -> Result<Self> {
        let tokens = VecDeque::from(lexer::lex_script(input)?);
        Ok(Self {
            tokens,
            steps: Vec::new(),
            guard_stack: vec![Vec::new()],
            pending_guards: None,
            pending_inline_guards: None,
            pending_can_open_block: false,
            pending_scope_enters: 0,
            scope_stack: Vec::new(),
        })
    }

    pub fn parse(mut self) -> Result<Vec<Step>> {
        while let Some(token) = self.tokens.pop_front() {
            match token {
                Token::Guard(guard) => self.handle_guard_token(guard)?,
                Token::BlockStart { line_no } => self.start_block_from_pending(line_no)?,
                Token::BlockEnd { line_no } => self.end_block(line_no)?,
                Token::Command(cmd) => self.handle_command_token(cmd)?,
            }
        }

        if self.guard_stack.len() != 1 {
            bail!("unclosed guard block at end of script");
        }
        if let Some(pending) = &self.pending_guards
            && !pending.is_empty()
        {
            bail!("guard declared on final lines without a following command");
        }

        Ok(self.steps)
    }

    fn handle_guard_token(&mut self, guard: GuardToken) -> Result<()> {
        if let Some(Token::Command(cmd)) = self.tokens.front() {
            if cmd.line_no == guard.line_end {
                self.pending_inline_guards = Some(guard.groups);
                self.pending_can_open_block = false;
                return Ok(());
            }
        }
        self.stash_pending_guard(guard.groups);
        self.pending_can_open_block = true;
        Ok(())
    }

    fn handle_command_token(&mut self, token: LexedCommand) -> Result<()> {
        let inline = self.pending_inline_guards.take();
        self.handle_command(token.line_no, token.kind, inline)
    }

    fn stash_pending_guard(&mut self, groups: Vec<Vec<Guard>>) {
        self.pending_guards = Some(if let Some(existing) = self.pending_guards.take() {
            combine_guard_groups(&existing, &groups)
        } else {
            groups
        });
    }

    fn start_block_from_pending(&mut self, line_no: usize) -> Result<()> {
        let guards = self
            .pending_guards
            .take()
            .ok_or_else(|| anyhow!("line {}: '{{' without a pending guard", line_no))?;
        if !self.pending_can_open_block {
            bail!("line {}: '{{' must directly follow a guard", line_no);
        }
        self.pending_can_open_block = false;
        self.start_block(guards, line_no)
    }

    fn start_block(&mut self, guards: Vec<Vec<Guard>>, line_no: usize) -> Result<()> {
        let with_pending = if let Some(pending) = self.pending_guards.take() {
            combine_guard_groups(&pending, &guards)
        } else {
            guards
        };
        let parent = self.guard_stack.last().cloned().unwrap_or_default();
        let next = if parent.is_empty() {
            with_pending
        } else if with_pending.is_empty() {
            parent
        } else {
            combine_guard_groups(&parent, &with_pending)
        };
        self.guard_stack.push(next);
        self.scope_stack.push(ScopeFrame {
            line_no,
            had_command: false,
        });
        self.pending_scope_enters += 1;
        Ok(())
    }

    fn end_block(&mut self, line_no: usize) -> Result<()> {
        if self.guard_stack.len() == 1 {
            bail!("line {}: unexpected '}}'", line_no);
        }
        if self.pending_guards.is_some() {
            bail!(
                "line {}: guard declared immediately before '}}' without a command",
                line_no
            );
        }
        let frame = self
            .scope_stack
            .last()
            .cloned()
            .ok_or_else(|| anyhow!("line {}: scope stack underflow", line_no))?;
        if !frame.had_command {
            bail!(
                "line {}: guard block starting on line {} must contain at least one command",
                line_no,
                frame.line_no
            );
        }
        let step = self
            .steps
            .last_mut()
            .ok_or_else(|| anyhow!("line {}: guard block closed without any commands", line_no))?;
        step.scope_exit += 1;
        self.scope_stack.pop();
        self.guard_stack.pop();
        Ok(())
    }

    fn guard_context(&mut self, inline: Option<Vec<Vec<Guard>>>) -> Vec<Vec<Guard>> {
        let mut context = if let Some(top) = self.guard_stack.last() {
            top.clone()
        } else {
            Vec::new()
        };
        if let Some(pending) = self.pending_guards.take() {
            context = if context.is_empty() {
                pending
            } else {
                combine_guard_groups(&context, &pending)
            };
            self.pending_can_open_block = false;
        }
        if let Some(inline_groups) = inline {
            context = if context.is_empty() {
                inline_groups
            } else {
                combine_guard_groups(&context, &inline_groups)
            };
        }
        context
    }

    fn handle_command(
        &mut self,
        _line_no: usize,
        kind: StepKind,
        inline_guards: Option<Vec<Vec<Guard>>>,
    ) -> Result<()> {
        let guards = self.guard_context(inline_guards);
        let scope_enter = self.pending_scope_enters;
        self.pending_scope_enters = 0;
        for frame in self.scope_stack.iter_mut() {
            frame.had_command = true;
        }
        self.steps.push(Step {
            guards,
            kind,
            scope_enter,
            scope_exit: 0,
        });
        Ok(())
    }
}

pub fn parse_script(input: &str) -> Result<Vec<Step>> {
    ScriptParser::new(input)?.parse()
}

fn combine_guard_groups(a: &[Vec<Guard>], b: &[Vec<Guard>]) -> Vec<Vec<Guard>> {
    if a.is_empty() {
        return b.to_vec();
    }
    if b.is_empty() {
        return a.to_vec();
    }
    let mut combined = Vec::new();
    for left in a {
        for right in b {
            let mut merged = left.clone();
            merged.extend(right.clone());
            combined.push(merged);
        }
    }
    combined
}
