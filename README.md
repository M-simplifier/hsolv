# hsolv

Ultra-fast symbolic and numeric math system with a strict, typed expression core.

## Quick start

```
cabal build
cabal run hsolv
```

## Rich TUI

```
cabal run hsolv-tui
```

Key bindings: Enter=run, Tab=complete, Up/Down=history, Esc/Ctrl-C=quit.

Note: `hsolv-tui` depends on `vty`, which needs the system `tinfo` library (e.g. `libtinfo-dev`).

## Learning Book (mdBook)

```
mdbook build book
mdbook serve book
```

## UX highlights

- Command-driven REPL with predictable outputs and stable pretty-printing.
- Fast suggestions with `:suggest <prefix>`.
- Inline documentation via `:doc <topic>`.

## Commands

- `:help`
- `:doc <topic>`
- `:suggest <prefix>`
- `simplify <expr>`
- `diff <var> <expr>`
- `solve <var> <expr>` (real roots only)
- `eval <expr> with x=1 y=2`
- `pretty <expr>`
- `:quit`

## Expression syntax

- Numbers and variables: `x`, `y1`, `z_2`
- Operators: `+ - * / ^`
- Functions: `sin`, `cos`, `tan`, `exp`, `log`, `sqrt`, `abs`
- Constants: `pi`, `e`
- Booleans: `true`, `false`, `&&`, `||`, `!`
- Comparisons: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Conditionals: `if <bool> then <num> else <num>`

## Examples

```
simplify (x + 0) * 1
diff x sin(x^2)
solve x x^2 - 5*x + 6
eval x^2 + 2*x + 1 with x=3
pretty if x > 0 then x else -x
```

## Notes on performance

The core data types are strict and use GADTs + DataKinds to enforce numeric vs boolean expressions. 
Parsing and simplification avoid backtracking-heavy operations for fast throughput.
