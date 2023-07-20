# `PascalScript`

## Introduction

`PascalScript` should be a (Turbo) Pascal inspired __interpreted__ language written in C ([C17](https://en.wikipedia.org/wiki/C17_(C_standard_revision))) using `lex` and `yacc` (or `bison`).

At first, a simple CLI should be implemented:

```bash
pascalscript hello.pas
```

And the traditional `hello.pas` should be like:

```pascal
program hello;
begin
  writeln('Hello, world!');
end;
```

In the future, it should be embeddable in other projects, like lua.

Examples __must__ be compilable with Free Pascal `fpc`, so we have sort of of an authoritative reference implementation.

## Links

- <https://delphi.fandom.com/wiki/Object_Pascal_Grammar>
- <https://www.cs.kent.edu/~durand/CS43101Fall2004/resources/Pascal-EBNF.html>
- <http://www.felix-colibri.com/papers/compilers/pascal_grammar/pascal_grammar.html>
- <http://www.bitsavers.org/pdf/borland/turbo_pascal/>
- <https://github.com/kdakan/Building-a-Pascal-Compiler>
