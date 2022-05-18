# MIPS

MIPS assembler and interpreter. WORK IN PROGRESS.

## Installation

1. [Install rust](https://rustup.rs/)

2. Install `mips`:

```
cargo install --git https://github.com/linuskmr/mips.git
```

3. Validate your installation. Running `mips` should print usage information.

## Running

The basic assembly syntax is as follows:

```
addi $t0 $s1 42
```

So operation first, then all arguments separated by whitespace.

Note: This is the same for `lw` and `sw` instructions, so instead of `lw $t0 42($s0)` you have to write `lw $t0 42`. This is due to simpler parsing.

### Assembler

Converts the assembler text into machine words. The machine words are printed on stdout. A debug representation is printed to stderr.

If you do not want the debug representation, you can forward stderr to `/dev/null` via `mips asm 2> /dev/null`.

`mips asm`: Read from stdin until EOF (CTRL+D).

`mips FILE asm`: Read from file.

## Interpreter

Executes the instructions and prints the contents of the registers after execution.

`mips exec`: Read from stdin until EOF (CTRL+D).

`mips FILE exec`: Read from file.

