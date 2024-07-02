#!/bin/env bash
cargo run build --target "x86_64-linux-gnu" -o app.o $1 && gcc -no-pie -o a.out app.o && LD_BIND_NOW=y && ./a.out; echo $?
