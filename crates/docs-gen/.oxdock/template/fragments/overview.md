## Overview

`docs-gen` renders text files from OxDock templates. Each target
declares ordered stages (verbatim or expanded file fragments) filled
with per-target values over global defaults. Targets are discovered
from every crate's `.oxdock/template` directory: no registry, no
hardcoded paths, any output format.

Run it with `cargo run -p docs-gen`. It accepts two flags:

- `--config <path>`: which config file to load. Without it, the runner
  looks for `./docs-gen.json`, then `./.oxdock/docs-gen.json`, and
  errors out when neither exists.
- `--root <path>`: which workspace root to render. Without it, the
  runner walks up from the current directory to the enclosing
  repository.
