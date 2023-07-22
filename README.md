# `PascalScript`

## Introduction

`PascalScript` should be a (Turbo) Pascal inspired __interpreted__ language written in C ([C17](https://en.wikipedia.org/wiki/C17_(C_standard_revision))) using `lex` and `yacc` (or `bison`).

At first, a simple CLI should be implemented under GNU/Linux:

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

In the future, it should be embeddable in other projects, like lua is for example.

Examples __must__ be compilable with Free Pascal `fpc`, so we have sort of of an authoritative reference implementation.

## Links

- <https://delphi.fandom.com/wiki/Object_Pascal_Grammar>
- <https://www.cs.kent.edu/~durand/CS43101Fall2004/resources/Pascal-EBNF.html>
- <http://www.felix-colibri.com/papers/compilers/pascal_grammar/pascal_grammar.html>
- <http://www.bitsavers.org/pdf/borland/turbo_pascal/>
- <https://github.com/kdakan/Building-a-Pascal-Compiler>
- <https://standardpascal.org/>

## Intended features

There should several steps before we get a "final" product.

### Integer only

This will make the base for the lexer, the tokenizer and the interpreter itself.

Just constants, variables, arithmetical expressions, a single parameter `WriteLn`, and comments.

- Keywords: `program` `const` `var` `integer` `begin` `end` `WriteLn` `=` `:=` `:` `;` `,` `{` `}` `(*` `*)`
- Identifiers: `[a-z|A-Z][a-z|A-Z|0-9]*` (no underscore at first)
- Integer constants: `[0-9]*`
- Operators: `+` `-` `*` `div` `mod` (`/` will come with real type)

```pascal
             1         2         3         4         5         6         7         8         9        10        11
    12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
 1  program step1; (* will we go above 9? *)
 2  const foo = 1;  { No negative }
 3  var a: integer; { One at a time }
 3  var b: integer;
 3  var c: integer;
 4  begin
 5    a := foo;
 6    b := 2;
 7    c := a + b;
 8    WriteLn(c);
 9    d := a * b div c; { will throw an error "Undeclared identifier 'd' at line 9, column 3" and stop execution }
10    foo := 12;        { will throw an error "Constant 'foo' cannot be assigned at line 10, column 3" and stop execution }
11  end.
```

Remarks;

- Comments will be paired, beginning with `{` means we go until `}`, no mix with `(*` and `*)`, so they can be imbricated on one level

Improvements to this first sight:

- Negative constants so we don't mess with an unary operator
- `var a, b, c: integer;` should be implemented
- Allow `_` in identifiers

### Conditional

New keywords: if then else

New operators: `<` `>` `<=` `>=` `<>` `and` `or` `not` (`=` with a different meaning is already there for constants)

```pascal
 1  program step2; (* will we go above 9? *)
 2  var a, b, c: integer;
 3  begin
 4    a := 1;
 5    b := 2;
 6    c := a + b;
 7    if not(c <= 3) then { means c > 3 but we should illustrate not unary operator }
 8    begin
 9      WriteLn(1);
10      WriteLn(c);
11    end { no ;? }
12    else
13      WriteLn(0);
14  end.
```

NB: no booleans mean false is zero, true is not zero.

### Loops

New keywords: while do repeat until

```pascal
TODO!
```

### Functions and procedures

This means we have local variables, and recursive calls.

```pascal
 1  program step3;
 2  var a: integer;
 3  function fact(n: integer): integer;
 4  var f : integer;
 5  begin
 6    if n < 1 then
 7      f := 1;
 8    else
 9      f := n * fact(n - 1);
10    fact := f;
11  end;
12  begin
13    a := fact(5);
14    WriteLn(a);
15  end.
```

### Further features

#### Case statement

New

```pascal
  ...
  case x of
    1: a := 1;
    2: a := 2;
    3..5: a := 3; { not at first sight? }
    else
      a := 34;
  end;
  ...
```

This may wait until we have implemented range types.

## Stack based VM or simpler runtime status?

Should we implement a stack based VM to execute code, and make the interpreter interact with this VM?

Or should we have a simpler "runtime status" like:

- `program`: source code of the program to execute
- `symbols`: hashtable with lower case key for the name, and an integer as value for now

global / local scope and other value types (real, boolean, string, function, procedure, ...) should come at their own time.
