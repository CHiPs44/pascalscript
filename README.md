# `PascalScript`

## Introduction

`PascalScript` should be a Turbo Pascal inspired __interpreted__ language written in C ([C17](https://en.wikipedia.org/wiki/C17_(C_standard_revision))) using `lex` and `yacc` (or `bison`).

At first, a simple CLI should be implemented (under GNU/Linux):

```bash
pascalscript < hello.pas
# with UUOC (useless use of cat ;-))
cat hello.pas | pascalscript
```

And the traditional `hello.pas` should be like:

```pascal
program Hello;
begin
  WriteLn('Hello, world!');
end.
```

In the future, it should be embeddable in other projects, like lua is for example.

Examples __must__ be compilable with Free Pascal `fpc`, so we have sort of an authoritative reference implementation.

## Links to seemingly useful documentations

### Pascal

- "Object Pascal Grammar" <https://delphi.fandom.com/wiki/Object_Pascal_Grammar>
- "Pascal EBNF" <https://www.cs.kent.edu/~durand/CS43101Fall2004/resources/Pascal-EBNF.html>
- "Pascal grammar" <http://www.felix-colibri.com/papers/compilers/pascal_grammar/pascal_grammar.html>
- "Turbo Pascal documentations PDF" <http://www.bitsavers.org/pdf/borland/turbo_pascal/>
- "building a Pascal compiler" <https://github.com/kdakan/Building-a-Pascal-Compiler>
- "Standard Pascal" <https://standardpascal.org/>
- "Free Pascal Reference guide" <https://www.freepascal.org/docs-html/ref/ref.html>

### `lex` / `flex` and `yacc` / `bison` stuff

- "lex et yacc" (french) <https://pageperso.lis-lab.fr/alexis.nasr/Ens/Compilation/cmX_lex_yacc.pdf>
- "Flex (Fast Lexical Analyzer Generator)" <https://www.geeksforgeeks.org/flex-fast-lexical-analyzer-generator/>
- "Introduction to YACC" <https://www.geeksforgeeks.org/introduction-to-yacc/>

## Features

There will be many steps before we get a "final" product.

### Integer only version

This will make the base for the lexer, the tokenizer and the interpreter itself.

Features should be:

- Integer constants
- Integer variables
- Arithmetical expressions
- A single integer parameter `WriteLn`
- Comments

Integer type will be the default of C `int` type.

Language elements are limited to:

- Keywords: `program` `const` `var` `integer` `begin` `end` `WriteLn`
- Symbols:  `=` `:=` `:` `;` `,` `{` `}` `(*` `*)`
- Identifiers: `[a-z|A-Z|_][a-z|A-Z|0-9|_]*`
- Integer constants: `[0-9]*` (positive)
- Operators: `+` `-` `*` `/` (`/` will be used as `div` for now, and `mod` will wait a bit)

```pascal
             1         2         3         4         5         6         7         8         9        10        11        12
    123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
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

- Negative constants / unary operator `-`
- `var a, b, c: integer;` should be implemented 

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

### Procedures (`+++`)

This means we have local variables.

```pascal
 1  program step3;
 2  var a: integer;
 3  procedure sum(a: integer, b: integer, var c: integer);
 4  begin
 5    c := a + b;
 6  end;
 7  begin
 8    sum(12, 34, a);
 9    WriteLn(a);
10  end.
```

### Functions

This means we have recursive calls.

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

### Other features

### Types

- Ranges (`..`)
- Enums
- Arrays
- Char (ASCII / ANSI)
- Strings (ASCII / ANSI)
- Sets
- Reals
- Pointers (`^`, `@`, ...)
- Byte (8), word (16), long (32) and longlong (64)

#### Case statement

This will wait until we have implemented range types.

```pascal
  ...
  case x of
    1: a := 1;
    2: a := 2;
    3..5: a := 3;
    else
      a := 34;
  end;
  ...
```

### Standard library

- Write / WriteLn for all types
- UTF-8 support

### Units

This would make PascalScript much more usable and extensible.

### Objects

- Turbo Pascal 5.5 syntax

### Extensions

- File I/O with calls more POSIX like instead of standard Pascal?
  - fopen / fclose / fread / fwrite / lseek / ...

## Stack based VM or simpler runtime status?

Should we implement a stack based VM to execute code, and make the interpreter interact with this VM?

Or should we have a simpler "runtime status" like:

- `program`: source code of the program to execute
- `symbols`: hashtable with lower case key for the name, an integer as value, and some flags
  - constant / variable flag
  - global / local scope and other value types (real, boolean, string, function, procedure, ...) should come at their own time
  - ...
