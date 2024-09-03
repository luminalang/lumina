# Installation

## Compiling from scratch

Clone the repository

```bash
$ git clone https://github.com/luminalang/lumina.git
```

Compile and install the compiler

```bash
$ cd lumina/
$ cargo build --release
$ sudo mv target/release/lumina /usr/bin/
```

Copy the `luminapath` directory containing the Lumina libraries to a suitable runtime folder and point the `$LUMINAPATH` environment variable to it.

```bash
$ cp -r luminapath/ $HOME/.local/share/lumina

# If you're using bash
$ echo "export LUMINAPATH=$HOME/.local/share/lumina/" >> $HOME/.bashrc
```
