### Workspaces start ephemeral

Scripts start in an ephemeral snapshot workspace, an isolated temp dir that leaves the source tree untouched. Pull inputs with `COPY` or `COPY_GIT`. Switch to the local directory with `WORKSPACE LOCAL` when the script should mutate in place.

```oxdock
WRITE snap.txt from-snapshot
ASSERT_FILE snap.txt from-snapshot
WORKSPACE LOCAL
WRITE local.txt from-local
ASSERT_FILE local.txt from-local
```

