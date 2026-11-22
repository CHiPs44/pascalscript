# Status

As of 2026-11-22, the AST based interpreter is able to run Pascal programs with the following constructs.

[EBNF grammar](pascalscript.ebnf) is up to date, too.

## Development state

- The lexer is split into dedicated number, string, and core lexer modules.
- Console input supports `Read`, `ReadLn`, and `ReadStr`.
- File-oriented input and output remain to be implemented.
- Documentation and the remaining system procedures are under active work.

## Data types

- `Integer`, `Unsigned`, `Real`, `Boolean`, `Char`
- `String`: fixed-size buffers (max 255 chars, no garbage collection, a long running program will exhaust memory)
- `Array`: multi-dimensional arrays
- `Type` definition for subranges & enumerations

## Assignment

To variable from an expression with `:=`.

## Expressions

- Arithmetic: `+`, `-`, `*`, `/`, `DIV`, `MOD`
- Relational: `=`, `<>`, `<`, `<=`, `>`, `>=`
- Logical: `AND`, `OR`, `NOT`
- Bitwise: `SHL`, `SHR`, `AND`, `OR`, `XOR`, `NOT`
- Array item access with `[]`

## Writing values

- to standard output with `Write` / `WriteLn`
- to strings with `WriteStr`
- formatting with `:width` for integers, `:width:precision` for reals

## Reading values

- from standard input with `Read` / `ReadLn`
- from a string with `ReadStr`
- reading through files is not implemented yet

## Conditional

- `If` / `Then` / `Else`

## Loops

- `Repeat` / `Until`
- `While` / `Do`
- `For` / `To` / `Downto` / `Do`

## Type definitions

- Enumerations
- Subranges for `Char`, `Integer`, `Unsigned`, `Enum`

## System constants, procedures & functions

- see [System Library](doc/SYSTEM.md)

## User defined procedures and functions

- Parameters / arguments
  - `Var` parameters are not thoroughly tested, as are Arrays
  - passing arrays by value is not supported
- Recursivity works
- No forward declarations yet
