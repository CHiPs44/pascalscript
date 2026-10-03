# Type rules

Rules for result type of unary and binary operations and assignment are enforced when parsing the source code into the AST.

They are also checked or used when evaluating/executing the AST.

## Range checking

Range checking is a runtime option.

It applies to value copies and conversions into a target type, and to some specific operations.

Examples that will fail when range checking is enabled:

- assigning an Integer or Unsigned value outside a subrange to a variable of that subrange
- assigning a negative Integer to an Unsigned

Integer arithmetic has no explicit PascalScript runtime overflow check, even when range checking is enabled.

## Unary operators

| Operator | Type     | Result type |
| -------- | -------- | ----------- |
| `+`      | Integer  | Integer     |
| `+`      | Real     | Real        |
| `+`      | Unsigned | Unsigned    |
| `-`      | Integer  | Integer     |
| `-`      | Real     | Real        |
| `-`      | Unsigned | Integer     |
| `Not`    | Boolean  | Boolean     |
| `Not`    | Integer  | Integer     |
| `Not`    | Unsigned | Unsigned    |

Integer and Unsigned subranges are treated as Integer and Unsigned respectively, but are likely to fail when range checking is enabled.

Types not listed above are invalid, and will fail to parse.

```pascal
Program UnaryOperators;
Type
  D20Roll = 1..20;
Var
  I: Integer;
  U: Unsigned;
  R: Real;
  B: Boolean;
  D: D20Roll;
Begin
  I := 1;
  U := 1;
  R := 1.0;
  B := True;
  D := 1;
  { ---------- OK ---------- }
  I := +I;
  I := -I;
  U := +U;
  R := +R;
  R := -R;
  B := Not B;
  I := Not I;
  U := Not U;
  D := +D;
  { ---------- Compiler error during parsing ---------- }
  B := +B;
  B := -B;
  R := Not R;
  { ---------- Runtime range error when range checking is enabled ---------- }
  I := -U; { If U > MaxInt }
  U := -U;
  D := -D;
  D := Not D;
End.
```

## Binary operators

C Functions:

- [ps_parse_expression()](../src/ps_parse_expression.c)
- [ps_ast.c:ps_ast_binary_operation_get_result_type()](../src/ps_ast.c)
- [ps_ast_execute.c:ps_ast_evaluate_expression_binary()](../src/ps_ast_execute.c)

### Arithmetic operations

| Operator                | Left     | Right    | Result type |
| ----------------------- | -------- | -------- | ----------- |
| `+` `-` `*` `Div` `Mod` | Integer  | Integer  | Integer     |
| `+` `-` `*` `Div` `Mod` | Integer  | Unsigned | Integer     |
| `+` `-` `*` `Div` `Mod` | Unsigned | Integer  | Unsigned    |
| `+` `*` `Div` `Mod`     | Unsigned | Unsigned | Unsigned    |
| `-`                     | Unsigned | Unsigned | Integer¹    |
| `+` `-` `*` `/` `**`³   | Real     | Scalar²  | Real        |
| `+` `-` `*` `/` `**`³   | Scalar²  | Real     | Real        |

¹ Free Pascal has a special rule for this, see <https://www.freepascal.org/docs-html/current/ref/refsu4.html>.

### Bitwise operations

| Operator                     | Left     | Right    | Result type |
| ---------------------------- | -------- | -------- | ----------- |
| `And` `Or` `Xor` `Shl` `Shr` | Integer  | Integer  | Integer     |
| `And` `Or` `Xor` `Shl` `Shr` | Integer  | Unsigned | Integer     |
| `And` `Or` `Xor` `Shl` `Shr` | Unsigned | Integer  | Unsigned    |
| `And` `Or` `Xor` `Shl` `Shr` | Unsigned | Unsigned | Unsigned    |

### String concatenation

| Operator | Left   | Right  | Result type |
| -------- | ------ | ------ | ----------- |
| `+`      | Char   | Char   | String      |
| `+`      | Char   | String | String      |
| `+`      | String | Char   | String      |
| `+`      | String | String | String      |

### Boolean operations

All these yield a Boolean result.

| Operator                   | Left     | Right    |
| -------------------------- | -------- | -------- |
| `And` `Or` `Xor`           | Boolean  | Boolean  |
| `=` `<>`                   | Boolean  | Boolean  |
| `=` `<>` `<` `<=` `>` `>=` | Char     | Char     |
| `=` `<>` `<` `<=` `>` `>=` | Char     | String   |
| `=` `<>` `<` `<=` `>` `>=` | String   | Char     |
| `=` `<>` `<` `<=` `>` `>=` | String   | String   |
| `=` `<>` `<` `<=` `>` `>=` | Integer  | Integer  |
| `=` `<>` `<` `<=` `>` `>=` | Integer  | Unsigned |
| `=` `<>` `<` `<=` `>` `>=` | Real     | Scalar²  |
| `=` `<>` `<` `<=` `>` `>=` | Scalar²  | Real     |
| `=` `<>` `<` `<=` `>` `>=` | Unsigned | Integer  |
| `=` `<>` `<` `<=` `>` `>=` | Unsigned | Unsigned |

² Scalar in this case is: Integer, Unsigned, Integer subrange, Unsigned subrange, Real

³ Infix `**` is not implemented. `Power(a, b)` is available as a function. The parser accepts numeric arguments, and the runtime converts both arguments to Real before calling `Power` (the same conversion is used by `LogN` in [ps_ast_execute_function_call_system_2arg()](../src/ps_ast_execute.c)).

## Type compatibility matrix

This table is for asssignment `:=` and parameter passing.

Same type for left and right should be always compatible, this is not shown in the table.

| Left / Right | Integer | Unsigned | Real | Char | String | Boolean | Array |
| ------------ | :-----: | :------: | :--: | :--: | :----: | :-----: | :---: |
| Integer      |   ✅    |    ✅    |  🚫  |  🚫  |   🚫   |   🚫    |  🚫   |
| Unsigned     |   ✅    |    ✅    |  🚫  |  🚫  |   🚫   |   🚫    |  🚫   |
| Real         |   ✅    |    ✅    |  ✅  |  🚫  |   🚫   |   🚫    |  🚫   |
| Char         |   🚫    |    🚫    |  🚫  |  ✅  |   🚫   |   🚫    |  🚫   |
| String       |   🚫    |    🚫    |  🚫  |  ✅  |   ✅   |   🚫    |  🚫   |
| Boolean      |   🚫    |    🚫    |  🚫  |  🚫  |   🚫   |   ✅    |  🚫   |
| Array        |   🚫    |    🚫    |  🚫  |  🚫  |   🚫   |   🚫    |  🚫   |

For now, arrays are not assignable nor passable by value.

Example:

```pascal
Program TypeCompatibility;

Type
  Days = (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
  Months = (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
  D100Roll = 1..100;

Var
  i: Integer;
  Day1, Day2: Days;
  d100: D100Roll;

Procedure Foo(a: Integer);
Begin
  {...}
End;

Procedure Bar(a: Real);
Begin
  {...}
End;

Begin
  { ---------- OK ---------- }
  i := 1;
  Day1 := Mon;
  Day2 := Day1;
  d100 := 42;
  Foo(1);
  Bar(1.0);
  Bar(1);
  { ---------- Compiler error during parsing ---------- }
  i := 1.0;
  Day1 := 1;
  Day1 := i;
  Day1 := Apr;
  Foo(1.0);
  { ---------- Runtime range error when range checking is enabled ---------- }
  i := Maxint + 1;
  d100 := 101;
End.
```
