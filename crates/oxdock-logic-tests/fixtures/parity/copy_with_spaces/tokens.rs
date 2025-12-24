COPY --from-current-workspace /workspace-dir/src_file.txt out/ws-target.txt
COPY ./build/source.txt out/build/target.txt
