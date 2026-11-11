# microgo-mips

Compiler for Micro Go, a small subset of Go, to MIPS assembly. Written in OCaml
with ocamllex and Menhir, as a two-person project for the compilation course at
Université Paris-Saclay (fall 2025).

It does the usual four steps: lexing, parsing, type checking, then code
generation. The output is a `.s` file you can run in MARS or SPIM.

## What the language looks like

```go
package main
import "fmt"

type node struct { val int; next *node }

func divmod(a, b int) (int, int) {
    q := 0
    for a >= b { q++; a = a - b }
    return q, a
}

func main() {
    var head *node = nil
    for i := 1; i <= 4; i++ {
        n := new(node)
        n.val = i * i
        n.next = head
        head = n
    }
    sum := 0
    for p := head; p != nil; p = p.next { sum += p.val }
    q, r := divmod(sum, 7)
    fmt.Print("sum=", sum, " q=", q, " r=", r, "\n")
}
```

This prints `sum=30 q=4 r=2`.

Supported: `int`, `bool`, `string` and pointers to structs, functions with
several return values, `:=` with type inference, `if`/`else`, the different
forms of `for`, and `fmt.Print`. Like in Go, semicolons at the end of lines are
optional. The compiler also warns about variables that are never used.

## Build and run

You need OCaml, dune and Menhir.

```sh
make
./mgoc.exe tests/base/arith.go     # writes tests/base/arith.s
```

Then load the `.s` file in MARS, or run `spim -file tests/base/arith.s`.
`--parse-only` and `--type-only` stop after the corresponding step.

The `tests` folder has valid programs (`base`, `torture`) and programs that
must be rejected (`fail`).

## Files

- `mgolexer.mll`: lexer
- `mgoparser.mly`: grammar
- `typechecker.ml`: type checking
- `compile.ml`: MIPS code generation
- `mgoc.ml`: command line

The full report (in French) is in `Rapport-ARAKELIAN-LI.pdf`.

## Authors

Raphaël Arakelian and Juralaine Li
