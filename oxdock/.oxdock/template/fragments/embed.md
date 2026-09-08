## Embed at compile time

The whole idea in one example: write a script, run it during `rustc`,
and read its artifacts from the binary at runtime — no containers, no
codegen step, no heap allocation:

