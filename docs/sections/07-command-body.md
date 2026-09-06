### WITH_IO

Reroute standard streams.

**Syntax:** `WITH_IO [bindings] <command> | WITH_IO [bindings] { <commands> }`

Reroutes the standard streams of the next command or, in block form, of every enclosed command. Bindings map streams (`stdin`, `stdout`, `stderr`) to named pipes (`stdout=pipe:name`). Pipe names registered by the host runtime tee structured output elsewhere; a name bound as output can later feed another command's `stdin`, connecting commands without touching the terminal. Nested blocks stack defaults; inline bindings override inherited ones for their command only; closing a block restores previous wiring.

**Examples:**

**Example: with_io block**

```oxdock
WITH_IO [stdout=pipe:log] {
  ECHO first
  ECHO second
}
WITH_IO [stdin=pipe:log] WRITE captured.txt
```


### FOR

Iterate over a list or map.

**Syntax:** `FOR $item IN <expr> { <commands> } | FOR $key, $value IN <expr> { <commands> }`

The loop variable receives each element (lists) or value (maps); with two variables, the first receives the key.

**Examples:**

**Example: for loop**

```oxdock
LET $items = ["a", "b"]
FOR $item IN $items {
  ECHO $item
}

LET $map = {"x": 1}
FOR $k, $v IN $map {
  ECHO "$k=$v"
}
```


### IF

Conditional execution.

**Syntax:** `IF <expr> { <commands> } [ELSE IF <expr> { <commands> }] [ELSE { <commands> }]`

The condition is evaluated as a boolean expression. Prefix `!` negates (`IF !false`); only Bool values are accepted as conditions.

**Examples:**

**Example: if else**

```oxdock
IF true {
  ECHO yes
} ELSE {
  ECHO no
}

IF false {
  ECHO skipped
} ELSE IF true {
  ECHO fallback
}

IF !false {
  ECHO inverted
}
```


### LET

Bind script-local variables.

**Syntax:** `LET $var = <expr> | LET $var = ASYNC { <commands> }`

Assigns a value to a script-local variable. Variables are usable in templates (`{{ $var }}`), guards, and expressions. With `ASYNC`, spawns a background task and stores its handle (see ASYNC).

**Examples:**

**Example: let**

```oxdock
LET $name = "world"
ECHO "hello, {{ $name }}"

LET $items = ["a", "b"]
LET $count = 42
```


### ASYNC

Run steps in a background thread.

**Syntax:** `ASYNC <command...> | ASYNC { <commands> } | LET $var = ASYNC { <commands> }`

Runs a command or block of commands in a background thread with subshell isolation. Mutations (ENV, WORKDIR) stay within the block. With `LET`, stores a task handle for `AWAIT`.

**Examples:**

**Example: async**

```oxdock
ASYNC ECHO "first"

ASYNC {
    ECHO "first"
    ECHO "second"
}
```

**Example: async task handle**

```oxdock
LET $task = ASYNC {
    ECHO "built"
}
AWAIT $task
```


### AWAIT

Join a background task.

**Syntax:** `AWAIT $var`

Blocks until the named task completes. Propagates errors if the task failed.

**Examples:**

**Example: await**

```oxdock
LET $task = ASYNC ECHO "done"
AWAIT $task
```


### CANCEL

Synchronously cancel a background task.

**Syntax:** `CANCEL $var`

Kills the named background task spawned via LET $var = ASYNC .... Blocking: returns only after the task thread has been joined and its OS process reaped, so no residual filesystem or stream mutation follows. A later AWAIT $var reports cancellation. Only named tasks can be cancelled.

**Examples:**

**Example: cancel**

```oxdock
LET $task = ASYNC SLEEP 30s
CANCEL $task
```


### TIMEOUT

Enforce an execution deadline.

**Syntax:** `TIMEOUT <duration> <command...> | TIMEOUT <duration> { <commands> } | TIMEOUT <duration> AWAIT $var`

Aborts the wrapped step or block with a deadline error if it exceeds the duration (e.g. 500ms, 10s, 2m; a bare number means seconds). A blocking foreground process is killed.

**Examples:**

**Example: timeout**

```oxdock
TIMEOUT 30s WRITE heartbeat.txt alive
```

**Example: timeout block**

```oxdock
TIMEOUT 30s {
    WRITE a.txt one
    WRITE b.txt two
}
```


### WORKDIR

Change the working directory.

**Syntax:** `WORKDIR <path>`

Sets the current working directory.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `path` | `string` | yes | Directory to change to |

**Examples:**

**Example: change working directory**

```oxdock
WORKDIR project/src
WRITE generated.txt generated-under-workdir
ASSERT_FILE generated.txt generated-under-workdir
```


### WORKSPACE

Switch workspace roots.

**Syntax:** `WORKSPACE SNAPSHOT|LOCAL`

SNAPSHOT or LOCAL root.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `target` | `SNAPSHOT|LOCAL` | yes | Target root |

**Examples:**

**Example: switch roots**

```oxdock
WORKSPACE LOCAL
```


### ENV

Set an environment variable.

**Syntax:** `ENV KEY=value`

Inserts or updates an env var.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `assignment` | `KEY=value` | yes | KEY=value pair |

**Examples:**

**Example: set env**

```oxdock
ENV APP_MODE=production
```


### INHERIT_ENV

Inherit env vars from host.

**Syntax:** `INHERIT_ENV <key>...`

Declares which host environment variables to inherit into the script. Must appear before any other commands and at most once. Without this directive, the script starts with an empty environment.

**Examples:**

**Example: inherit env**

```oxdock
INHERIT_ENV [PATH, HOME]
```


### ECHO

Print to stdout.

**Syntax:** `ECHO <message>`

Outputs message to stdout.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `message` | `string` | yes | Text |

**Output:** Stdout

**Examples:**

**Example: echo**

```oxdock
ECHO build-complete
```


### RUN

Execute shell command.

**Syntax:** `RUN <command...>`

Runs command in cwd.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `command` | `string...` | yes | Command |

**Examples:**

**Example: run**

```oxdock
RUN echo hello
```


### COPY

Copy file into workspace.

**Syntax:** `COPY [--from-current-workspace] <from> <to>`

Copies from host.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `from` | `path` | yes | Source |
| `to` | `path` | yes | Dest |

**Flags:**

| Flag | Type | Description |
| --- | --- | --- |
| `--from-current-workspace` | Flag | From workspace root |

**Examples:**

**Example: copy**

```oxdock roots:unified
WRITE src.txt content
COPY src.txt dst.txt
ASSERT_FILE dst.txt content
```


### COPY_GIT

Copy from git revision.

**Syntax:** `COPY_GIT [--include-dirty] <rev> <src> <dst>`

Checkout and copy.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `rev` | `string` | yes | Rev |
| `src` | `path` | yes | Src |
| `dst` | `path` | yes | Dst |

**Flags:**

| Flag | Type | Description |
| --- | --- | --- |
| `--include-dirty` | Flag | Include dirty |

**Examples:**

**Example: git copy**

```oxdock expect_error:"COPY source missing"
COPY_GIT HEAD src.txt dst.txt
```


### SYMLINK

Create symlink.

**Syntax:** `SYMLINK <from> <to>`

Creates symlink.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `from` | `path` | yes | Target |
| `to` | `path` | yes | Link |

**Examples:**

**Example: symlink**

```oxdock roots:unified
WRITE original.txt content
SYMLINK original.txt link.txt
ASSERT_FILE link.txt content
```


### MKDIR

Create directory.

**Syntax:** `MKDIR <path>`

Creates dir with parents.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `path` | `path` | yes | Dir path |

**Examples:**

**Example: mkdir**

```oxdock
MKDIR deeply/nested/tree
```


### LS

List directory.

**Syntax:** `LS [<path>]`

Lists entries.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `path` | `path` | no | Dir |

**Output:** Stdout

**Examples:**

**Example: ls**

```oxdock
MKDIR inventory
WRITE inventory/a.txt a
LS inventory
```


### CWD

Print working directory.

**Syntax:** `CWD`

Outputs cwd.

**Output:** Stdout

**Examples:**

**Example: cwd**

```oxdock
CWD
```


### READ

Read file to stdout.

**Syntax:** `READ [<path>]`

Outputs file contents.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `path` | `path` | no | File |

**Output:** Stdout

**Examples:**

**Example: read**

```oxdock
WRITE note.txt "hello"
READ note.txt
```


### READ_LINE

Read one line from stdin into a variable.

**Syntax:** `READ_LINE $var`

Reads bytes until newline without waiting for EOF, leaving the pipe open. Trailing newline is stripped (shell-read parity). On premature EOF assigns accumulated bytes and returns.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `var` | `$var` | yes | Variable to store the line |

**Examples:**

**Example: read line**

```oxdock
WITH_IO [stdout=pipe:lines] ECHO "first"
WITH_IO [stdin=pipe:lines] READ_LINE $reply
```


### WRITE

Write to file.

**Syntax:** `WRITE <path> [<contents>]`

Writes contents.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `path` | `path` | yes | File |
| `contents` | `string` | no | Content |

**Examples:**

**Example: write**

```oxdock
WRITE output.txt hello-world
```


### APPEND

Append to file.

**Syntax:** `APPEND <path> [<contents>]`

Appends contents.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `path` | `path` | yes | File |
| `contents` | `string` | no | Content |

**Examples:**

**Example: append**

```oxdock
WRITE log.txt line1
APPEND log.txt line2
ASSERT_FILE log.txt line1line2
```


### EXPAND

Expand templates.

**Syntax:** `EXPAND [<path>] [<KEY=val> ...]`

Expands placeholders.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `path` | `path` | no | Template |

**Output:** Stdout

**Examples:**

**Example: expand**

```oxdock
ENV NAME="Alice"
WRITE template.md "Hello {{ env:NAME }}!"
EXPAND template.md
ASSERT_STDOUT "Hello Alice!"
```


### ASSERT_FILE

Assert file exists.

**Syntax:** `ASSERT_FILE [--hash <sha256>] <path> [<expected>]`

Verifies file.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `path` | `path` | yes | File |
| `expected` | `string` | no | Expected |

**Flags:**

| Flag | Type | Description |
| --- | --- | --- |
| `--hash` | String | SHA-256 |

**Examples:**

**Example: assert file**

```oxdock
WRITE payload.bin stable-content
ASSERT_FILE payload.bin stable-content
```


### ASSERT_DIR

Assert dir exists.

**Syntax:** `ASSERT_DIR <path>`

Verifies dir.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `path` | `path` | yes | Dir |

**Examples:**

**Example: assert dir**

```oxdock
MKDIR dist/assets
ASSERT_DIR dist/assets
```


### ASSERT_ABSENT

Assert path absent.

**Syntax:** `ASSERT_ABSENT <path>`

Verifies absence.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `path` | `path` | yes | Path |

**Examples:**

**Example: assert absent**

```oxdock
ASSERT_ABSENT missing.txt
```


### ASSERT_STDOUT

Assert stdout contains.

**Syntax:** `ASSERT_STDOUT <substring>`

Verifies stdout.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `substring` | `string` | yes | Substring |

**Examples:**

**Example: assert stdout**

```oxdock
ECHO build-complete
ASSERT_STDOUT build-complete
```


### HASH_SHA256

Print SHA-256.

**Syntax:** `HASH_SHA256 <path>`

Computes digest.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `path` | `path` | yes | File |

**Output:** Stdout

**Examples:**

**Example: hash**

```oxdock
WRITE payload.txt hello
HASH_SHA256 payload.txt
```


### EXIT

Exit pipeline.

**Syntax:** `EXIT <code>`

Stops the pipeline immediately with an `EXIT requested with code <code>` error; steps after it never run, at any nesting depth. Enclosing blocks still unwind their LET/ENV/WORKDIR/WORKSPACE state, anonymous background tasks are killed synchronously, and files written before the EXIT persist.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `code` | `int` | yes | Code |

**Examples:**

**Example: exit**

```oxdock expect_error:"EXIT requested with code 0"
EXIT 0
```


### SLEEP

Sleep without spawning a shell.

**Syntax:** `SLEEP <duration>`

Parks the step for the duration (e.g. 500ms, 10s, 2m). Cooperative: checks for cancellation so an enclosing TIMEOUT or task teardown interrupts the sleep. Cross-platform alternative to shell sleep for testing time boundaries.

**Arguments:**

| Name | Type | Required | Description |
| --- | --- | --- | --- |
| `duration` | `duration` | yes | How long to sleep |

**Examples:**

**Example: sleep**

```oxdock
SLEEP 100ms
```


