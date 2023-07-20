# `PascalScript`

`PascalScript` should be a (Turbo) Pascal inspired interpreted language written in C ([C17](https://en.wikipedia.org/wiki/C17_(C_standard_revision))) using lex and yacc or bison.

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

## Links

- <https://delphi.fandom.com/wiki/Object_Pascal_Grammar>
- <https://www.cs.kent.edu/~durand/CS43101Fall2004/resources/Pascal-EBNF.html>
- <http://www.felix-colibri.com/papers/compilers/pascal_grammar/pascal_grammar.html>
- <http://www.bitsavers.org/pdf/borland/turbo_pascal/>
