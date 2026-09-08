## Common usage

Run a script file (positional, same as `--script`):

```sh
oxdock ./build.oxfile
```

Run a script file (explicit flag form):

```sh
oxdock --script ./build.oxfile
```

Pipe a script into the CLI:
```sh
cat my-script.oxfile | oxdock
```

Drop into a shell inside the temporary workspace (interactive):
```sh
oxdock --shell
```

