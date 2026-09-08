### Stream bytes between steps

`WITH_IO` routes stdout into named pipes and back into stdin, so steps form custom pipelines without temp files.

```oxdock
WITH_IO [stdout=pipe:msg] ECHO piped-bytes
WITH_IO [stdin=pipe:msg] WRITE piped.txt
READ piped.txt
ASSERT_STDOUT piped-bytes
```

