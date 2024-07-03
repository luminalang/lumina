#!/bin/env bash
    
# TODO: rename target to musl. Since well, it uses musl. If we link to musl for MMTK then we might as well expose it to Lumina

nasm -f elf64 -o assembly/x86_64-linux-syscall.o assembly/x86_64-linux-syscall.asm

cargo run -- build --target "x86_64-linux-mmtk_syscall" -o app.o $1 \
    && /usr/bin/x86_64-linux-musl-gcc -flto -static -no-pie -o a.out \
        app.o \
        lumina-gc/target/x86_64-unknown-linux-musl/release/liblumina_gc.a \
        assembly/x86_64-linux-syscall.o \
        && LD_BIND_NOW=y && ./a.out; echo $?
