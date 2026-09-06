TIMEOUT 10s RUN "echo hi"
TIMEOUT 500ms ECHO hello
TIMEOUT 2m {
    WRITE "a.txt" x
    ECHO done
}
TIMEOUT 30s AWAIT $task
WITH_IO [stdout=pipe:p] TIMEOUT 5s RUN "echo x"
SLEEP 100ms
