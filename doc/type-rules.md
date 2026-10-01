# Type rules

Rules for result type of unary and binary operations are enforced when parsing the source code into the AST.

They are also checked or used when evaluating the AST.

## Range checking

Range checking is a runtime option.

It applies to value copies and conversions into a target type, and to some specific operations.

Examples when range checking is enabled:

- assigning an Integer or Unsigned value outside a subrange to a variable of that subrange will fail
- assigning a negative Integer to an Unsigned will fail

Integer arithmetic has no explicit PascalScript runtime overflow check, even when range checking is enabled.

## Unary operators

Enforced in C Functions:

- [ps_parse_factor()](../src/ps_parse_expression.c)
- [ps_ast.c:ps_ast_unary_operation_get_result_type()](../src/ps_ast.c)
- [ps_ast_execute.c:ps_ast_evaluate_expression_unary()](../src/ps_ast_execute.c)

| Operator | Type              |    Result type    |
| :------: | :---------------- | :---------------: |
|   `+`    | Integer           |      Integer      |
|   `+`    | Integer subrange  | Integer subrange  |
|   `+`    | Real              |       Real        |
|   `+`    | Unsigned          |     Unsigned      |
|   `+`    | Unsigned subrange | Unsigned subrange |
|   `-`    | Integer           |      Integer      |
|   `-`    | Integer subrange  | Integer subrange  |
|   `-`    | Real              |       Real        |
|   `-`    | Unsigned          |      Integer      |
|   `-`    | Unsigned subrange |      Integer      |
|  `Not`   | Boolean           |      Boolean      |
|  `Not`   | Integer           |      Integer      |
|  `Not`   | Integer subrange  | Integer subrange  |
|  `Not`   | Unsigned          |     Unsigned      |
|  `Not`   | Unsigned subrange | Unsigned subrange |

Types not listed above are invalid, and will fail to parse.

One can not negate a string, for example.

## Binary operators

C Functions:

- [ps_parse_expression()](../src/ps_parse_expression.c)
- [ps_ast.c:ps_ast_binary_operation_get_result_type()](../src/ps_ast.c)
- [ps_ast_execute.c:ps_ast_evaluate_expression_binary()](../src/ps_ast_execute.c)

## Arithmetic operations

| Operator                | Left     | Right    | Result type |
| :---------------------- | :------- | :------- | :---------- |
| `+` `-` `*` `Div` `Mod` | Integer  | Integer  | Integer     |
| `+` `-` `*` `Div` `Mod` | Integer  | Unsigned | Integer     |
| `+` `-` `*` `Div` `Mod` | Unsigned | Integer  | Unsigned¹   |
| `+` `*` `Div` `Mod`     | Unsigned | Unsigned | Unsigned    |
| `-`                     | Unsigned | Unsigned | Integer¹    |
| `+` `-` `*` `/` `**`³   | Real     | Scalar²  | Real        |
| `+` `-` `*` `/` `**`³   | Scalar²  | Real     | Real        |

### Bit operations

| Operator         | Left     | Right    | Result type |
| :--------------- | :------- | :------- | :---------- |
| `And` `Or` `Xor` | Integer  | Integer  | Integer     |
| `And` `Or` `Xor` | Integer  | Unsigned | Integer     |
| `And` `Or` `Xor` | Unsigned | Integer  | Unsigned¹   |
| `And` `Or` `Xor` | Unsigned | Unsigned | Unsigned    |
| `Shl` `Shr`      | Integer  | Integer  | Integer     |
| `Shl` `Shr`      | Integer  | Unsigned | Integer     |
| `Shl` `Shr`      | Unsigned | Integer  | Unsigned    |
| `Shl` `Shr`      | Unsigned | Unsigned | Unsigned    |

### String concatenation

| Operator | Left   | Right  | Result type |
| :------- | :----- | :----- | :---------- |
| `+`      | Char   | Char   | String      |
| `+`      | Char   | String | String      |
| `+`      | String | Char   | String      |
| `+`      | String | String | String      |

### Boolean operations

All these yield a Boolean result.

| Operator                   | Left     | Right    |
| :------------------------- | :------- | :------- |
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

¹ cf. <https://www.freepascal.org/docs-html/current/ref/refsu4.html>

² Scalar in this case is: Integer, Unsigned, Integer subrange, Unsigned subrange, Real

³ Infix `**` is not implemented. `Power(a, b)` is available as a function. The parser accepts numeric arguments, and the runtime converts both arguments to Real before calling `Power` (the same conversion is used by `LogN` in [ps_ast_execute_function_call_system_2arg()](../src/ps_ast_execute.c)).

TODO

- The `Unsigned`-left, `Integer`-right order is inconsistent:
  - `+`, `-`, `*`, `Div`, `Mod`, and `Or` infer an `Unsigned` result in the AST but produce an `Integer` value at runtime.
  - The mismatch can cause an implicit conversion where the expression is consumed
- Comparisons infer `Boolean` without checking operand compatibility; unsupported pairs are rejected only at runtime.

## Compatibility

|       Left       |       Right       | Compatible? |
| :--------------: | :---------------: | :---------: |
|       Same       |       Same        |     Yes     |
|     Integer      |     Unsigned      |     Yes     |
|     Unsigned     |      Integer      |     Yes     |
|     Integer      | Integer subrange  |     Yes     |
|     Unsigned     | Unsigned subrange |     Yes     |
|     Integer      | Unsigned subrange |     Yes     |
| Integer subrange | Integer\|Unsigned |     Yes     |

TODO This compatibility table is not enforced as a general static rule. The helper in `ps_ast.c` has no call sites and only compares the top-level type tag: it can treat distinct subranges or enums as compatible, while rejecting different tags that assignment conversion supports. Assignments instead convert values at runtime; with range checking enabled, conversions can fail for negative Integer-to-Unsigned values, values outside a destination subrange, or Unsigned values too large for Integer. Enums require the exact same enum type. User-defined procedure and function arguments also require the exact same type definition, for both by-value and by-reference parameters.
