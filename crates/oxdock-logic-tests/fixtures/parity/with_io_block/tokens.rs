WITH_IO [stdout=pipe:block_outer] {
    ECHO "outer-1"
    WITH_IO [stdout] ECHO "override-stdout"
    WITH_IO [stdout=pipe:block_inner] {
        ECHO "inner-2"
    }
    ECHO "outer-3"
}

ECHO "outside"
