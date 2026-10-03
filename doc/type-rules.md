# Type rules

Rules for result type of unary and binary operations are enforced when parsing the source code into the AST.

They are also checked or used when evaluating the AST.

## Range checking

Range checking is a runtime option.

It applies to value copies and conversions into a target type, and to some specific operations.

Examples that will fail when range checking is enabled:

- assigning an Integer or Unsigned value outside a subrange to a variable of that subrange
- assigning a negative Integer to an Unsigned

Integer arithmetic has no explicit PascalScript runtime overflow check, even when range checking is enabled.

## Unary operators

Rules are checked in C Functions:

- [ps_ast.c:ps_ast_unary_operation_get_result_type()](../src/ps_ast.c)
- [ps_parse_factor()](../src/ps_parse_expression.c)
- [ps_ast_execute.c:ps_ast_evaluate_expression_unary()](../src/ps_ast_execute.c)

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

Types not listed above are invalid, and will fail to parse.

One can not negate a string, for example.

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

This table is for asssignment `:=` and parameter passing:

```pascal
Program TypeCompatibility;

Type
  Days = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
  Months = (January, February, March, April, May, June, July, August, September, October, November, December);

Var
  i: Integer;
  d1, d2: Days;

Procedure Foo(a: Integer);
Begin
  {...}
End;

Procedure Bar(a: Real);
Begin
  {...}
End;

Begin
  i := 1.0;     // Error: incompatible types
  i := 1;       // OK
  Foo(1.0);     // Error: incompatible types
  Foo(1);       // OK
  Bar(1.0);     // OK
  Bar(1);       // OK
  d1 := Monday; // OK
  d1 := 1;      // Error: incompatible types
  d1 := d2;     // OK
  d1 := i;      // Error: incompatible types
  d1 := April;  // Error: incompatible types
End.
```

Same type for left and right should be always compatible, this is not shown in the table.

Yes means the left type can accept the right type in an assignment or parameter passing operation.

| Left / Right | Integer | Unsigned | Real | Char | String | Boolean | Array |
| ------------ | :-----: | :------: | :--: | :--: | :----: | :-----: | :---: |
| Integer      |    ✓    |    ✓     |  ✗   |  ✗   |   ✗    |    ✗    |   ✗   |
| Unsigned     |    ✓    |    ✓     |  ✗   |  ✗   |   ✗    |    ✗    |   ✗   |
| Real         |    ✓    |    ✓     |  ✓   |  ✗   |   ✗    |    ✗    |   ✗   |
| Char         |    ✗    |    ✗     |  ✗   |  ✓   |   ✗    |    ✗    |   ✗   |
| String       |    ✗    |    ✗     |  ✗   |  ✓   |   ✓    |    ✗    |   ✗   |
| Boolean      |    ✗    |    ✗     |  ✗   |  ✗   |   ✗    |    ✓    |   ✗   |
| Array        |    ✗    |    ✗     |  ✗   |  ✗   |   ✗    |    ✗    |   ✗   |

For now, arrays are not assignable nor passable by value.
