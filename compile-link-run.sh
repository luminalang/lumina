#!/bin/env bash
    
# TODO: rename target to musl. Since well, it uses musl. If we link to musl for MMTK then we might as well expose it to Lumina

set -e

# arch="x86_64"
# platform="linux-musl"

arch=$1
platform=$2
src_dir=$3

if [ -z "$src_dir" ]; then
    echo "example: ./compile-and-run.sh x86_64 linux-musl sandbox/snippets"
    exit 1
fi

target="$arch-$platform"

LD_BIND_NOW=y 

cd lumina-gc
./build
cp target/$arch-*-$platform/debug/liblumina_gc.a "../libs/$target-liblumina_gc.a"
cd ..

if [[ $platform == *"linux"* ]]; then
    # TODO: 32-bit
    nasm -f elf64 -o libs/$target.o assembly/$target.asm
fi

cargo run -- build --target $target -o app.o $src_dir \

if [[ $platform == "linux-musl" ]]; then
/usr/bin/x86_64-linux-musl-gcc -z noexecstack -flto -static -no-pie -o a.out \
        app.o \
        libs/$target*.o \
        libs/$target*.a
fi

if [[ $platform == "linux-gnu" ]]; then
gcc -z noexecstack -flto -no-pie -o a.out \
        app.o \
        libs/$target*.o \
        libs/$target*.a \
        -lm
fi

set +e

./a.out
echo $?
