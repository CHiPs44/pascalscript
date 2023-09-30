# `PascalScript` - an interpreted Pascal dialect

## Introduction

`PascalScript` should be a Turbo Pascal inspired __interpreted__ language written in C ([C17](https://en.wikipedia.org/wiki/C17_(C_standard_revision))) using `lex` and `yacc` (in fact `flex` and `bison`).

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
- "Building a Pascal compiler" <https://github.com/kdakan/Building-a-Pascal-Compiler>
- "Standard Pascal" <https://standardpascal.org/>
- "Free Pascal Reference guide" <https://www.freepascal.org/docs-html/ref/ref.html>
- "Dragon compiler" <https://github.com/TimD1/DragonCompiler>
- "Simple Swift interpreter for the Pascal language" <https://github.com/igorkulman/SwiftPascalInterpreter>

### `lex` / `flex` and `yacc` / `bison` stuff

#### French

- "lex et yacc" (french) <https://pageperso.lis-lab.fr/alexis.nasr/Ens/Compilation/cmX_lex_yacc.pdf>
- "Mini manuel d'utilisation de Lex et Yacc" (french) <https://web.archive.org/web/20181009191604/http://www.linux-france.org/article/devl/lexyacc/minimanlexyacc.html#toc2>

#### English

- "Flex (Fast Lexical Analyzer Generator)" <https://www.geeksforgeeks.org/flex-fast-lexical-analyzer-generator/>
- "Introduction to YACC" <https://www.geeksforgeeks.org/introduction-to-yacc/>
- "ANSI C grammar" <https://www.lysator.liu.se/c/ANSI-C-grammar-y.html>
- "yacc-flex-interpreter" <https://github.com/koniecznyp/yacc-flex-interpreter>
- "Examples from flex itself" `/usr/share/doc/flex/examples/` and `/usr/share/doc/flex/examples/manual/` (at least on Debian based systems)
- "Examples from GNU Bison itself" `/usr/share/doc/bison/examples/c/`

## Features

There will be many steps before we get a "final" product.

### Step 1: integer only calculator, without any flow control

This will make the base for the lexer, the tokenizer and the interpreter itself.

Features are:

- Integer constants
- Integer variables
- Arithmetical expressions
- A single integer parameter procedure: `WriteLn`
- Comments

Integer type will be the default of C `int` type.

Language elements are limited to:

- Keywords: `program` `const` `var` `integer` `begin` `end` `WriteLn`
- Symbols:  `=` `:=` `:` `;` `,` `{` `}` `(*` `*)` `//`
- Identifiers: `[a-z|A-Z|_][a-z|A-Z|0-9|_]*`
- Integer constants: `[0-9]*` (positive)
- Operators: `+` `-` `*` `/` `div` `mod`

```pascal
program step1a;
const   foo = 1;
var     a: integer;
        b: integer;
        c: integer;
begin
        a := foo;
        b := 2;
        c := a + b;
        WriteLn(c);
        { will throw an error "Undeclared identifier 'd' at line L, column C" and stop execution }
        d := a * b div c;
        { will throw an error "Constant 'foo' cannot be assigned at line L, column C" and stop execution }
        foo := 12;
end. { . is mandatory }
```

Remarks:

- Comments will be paired, beginning with `{` means we go until `}`, no mix with `(*` and `*)`, so they can be imbricated on one level
- `//` one line comments came essentially for free when digging an already made set of rules for the lexical analyzer

Improvements to this first sight:

- `var a, b, c: integer;` should be implemented
- `Write` variant to output without a line break
- `const` could be used for string literals instead of integers only
- `Write` and `WriteLn` should accept a string constant as parameter

```pascal
program step1b;
const   foo = 1;
        msg = 'Result is: ';
var     a, b, c: integer;
begin
        a := foo;
        b := 2;
        c := a + b;
        Write(msg);
        WriteLn(c);
end.
```

### Step 2: Conditional

New keywords: `if` `then` `else`

New operators: `<` `>` `<=` `>=` `<>` `and` `or` `not` (`=` with a different meaning is already there for constants)

```pascal
program step2;
const   MSG1 = 'C is greater than 3.';
        MSG2 = 'C is less than 3.';
var     a, b, c: integer;
begin
        a := 1;
        b := 2;
        c := a + b;
        if not(c <= 3) then { means c > 3 but we should illustrate not unary operator ;-) }
        begin
          WriteLn(MSG1);
          WriteLn(c);
        end { no ; }
        else
          WriteLn(MSG2);
end.
```

NB: no booleans mean false is zero, true is not zero.

### Loops

New keywords: `while` `do` `repeat` `until` `for` `to` `downto`

```pascal
program step3;
var     i: integer;
begin
        i := 1;
        while i < 5 do
        begin
          WriteLn(i);
          i := i + 1;
        end;
        i := 1;
        repeat
          WriteLn(i);
          i := i + 1;
        until i > 5;
        for i := 9 downto 0 do
        begin
          WriteLn(i);
        end;
end.
```

NB:

- Implement `break` and `continue`?
- `for` loops will be improved later with `in` keyword for arrays and sets

### Procedures

This means we have input ("by value") and output ("by reference") parameters, local variables, and recursive calls.

```pascal
program step4a;

var a: integer;

procedure sum(a: integer, b: integer, var c: integer);
var     sum: integer;
begin
        sum := a + b;
        c := sum;
end;

begin
  sum(12, 34, a);
  WriteLn(a);
end.
```

### Functions

```pascal
program step4b;

var a: integer;

(* The "de-facto" standard of recursive functions *)
function fact(n: integer): integer;
var f: integer;
begin
  if n <= 1 then
    f := 1
  else
    f := n * fact(n - 1);
  fact := f;
end;

begin
  a := fact(5);
  WriteLn(a);
end.
```

### Base types

- `integer` is 32 bits signed type
- `real` is `double`, not `float`

### Other features

### Types

- Unsigned and signed integers:
  - `integer` is 32 bits signed type, period.
  - 8 bits: `Byte` / `Shortint`
  - 16 bits: `Word` / `Smallint`
  - 32 bits: `Longword` / `Longint`
  - 64 bits: `QWord` / `Int64`
- Ranges: `Min .. Max`
- Enums: `(One, Two, Three, Four)`
- Arrays: `Array[1..10] Of Integer`
- Char (see below)
- Strings (array of chars)
- Sets: `` (256 values max?)
- Records
- Pointers (`^`, `@`, ...)

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

### Characters and strings

- ASCII support only?
  - 128 to 255 is undefined behaviour
  - 1 byte per char
  - fixed length make string operations easy
  - maximum length: 255 (length is a byte, too)
- ANSI / Codepage support?
  - 437 for US and 850 for Western EU first
  - 1 byte per char
  - fixed length make string operations easy
  - maximum length: 255 (length is a byte, too)
- UTF-8 support?
  - all Unicode chars can be encoded
  - 1 byte per char for ASCII only text
  - variable length make string operations hard to implement
  - maximum length: 255 (length is a byte, too)
- UTF-16 support?
  - many Unicode chars can be encoded
  - 2 bytes per char for ASCII only text
  - fixed length make string operations easy
  - maximum length: 65535 (length is a 16 bits word, too)
- UTF-32 support?
  - all Unicode chars can be encoded
  - 4 bytes per char for ASCII only text
  - fixed length make string operations easy
  - maximum length: either4294967295 (length is a 32 bits word, too)

### Standard libraries

#### I/O

- Compatible:
  - Write / WriteLn for all base types, with variable number of arguments
- File I/O with more POSIX like calls instead of standard Pascal?
  - `fopen` / `fclose` / `fread` / `fwrite` / `lseek` / ...
- Format function with `{}` placeholders?
  - `Format('A={}, B={}', a, b)` with a=1 and b=2 should return `A=1, B=2`

#### Mathematical functions and constant(s)

- `sqrt`
- `sin` `cos` `tan` `asin` `acos` `atan` `pi`
- `ln` `log`
- ...

### Units

This would make PascalScript much more usable and extensible.

### Objects

Turbo Pascal 5.5 syntax should be enough.

### Extensions

- Variable length arrays?

## Stack based VM or simpler runtime status?

Should we implement a stack based VM to execute code, and make the interpreter interact with this VM?

Or should we have a simpler "runtime status" like:

- `program`: source code of the program to execute
- `symbol_table`: hashtable with lower case key for the name, an integer as value, and necessary data
  - kind: constant, variable, procedure, function, ...
  - global / local scope
  - other value types (real, boolean, string, function, procedure, ...) should come at their own time
  - ...

## License

This project is licensed under GNU General Public License 3.0 or later, see file `LICENSE`.

Each file should contains this header:

```c
/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/
```
