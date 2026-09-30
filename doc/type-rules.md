# Type rules

Range checking is a runtime option. It applies to value copies and conversions into a target type, and to some specific
operations; it is not a universal check on expression results.

Examples when range checking is enabled:

- assigning an Integer or Unsigned value outside a subrange to a variable of that subrange will fail
- assigning a negative Integer to an Unsigned will fail

Integer arithmetic has no explicit PascalScript runtime overflow check, even when range checking is enabled.

## Unary operators

cf. [ps_ast.c:ps_ast_unary_operation_get_result_type()](../src/ps_ast.c) and [ps_ast_execute.c:ps_ast_evaluate_expression_unary()](../src/ps_ast_execute.c)

| Operator | Type              | Result type       |
| :------: | :---------------- | :---------------: |
|   `+`    | Integer           | Integer           |
|   `+`    | Integer subrange  | Integer subrange  |
|   `+`    | Real              | Real              |
|   `+`    | Unsigned          | Unsigned          |
|   `+`    | Unsigned subrange | Unsigned subrange |
|   `-`    | Integer           | Integer           |
|   `-`    | Integer subrange  | Integer subrange  |
|   `-`    | Real              | Real              |
|   `-`    | Unsigned          | Integer           |
|   `-`    | Unsigned subrange | Integer           |
|  `Not`   | Boolean           | Boolean           |
|  `Not`   | Integer           | Integer           |
|  `Not`   | Integer subrange  | Integer subrange  |
|  `Not`   | Unsigned          | Unsigned          |
|  `Not`   | Unsigned subrange | Unsigned subrange |

TODO Unary `+` is parsed as a no-op and does not validate its operand type. Unary `-` and `Not` are evaluated at runtime;
they do not check that a result remains within the operand's subrange.

## Binary operators

cf. [ps_ast.c:ps_ast_binary_operation_get_result_type()](../src/ps_ast.c) and [ps_ast_execute.c:ps_ast_evaluate_expression_binary()](../src/ps_ast_execute.c)

|                 Operator                 |   Left   |  Right   | Result type |
| :--------------------------------------: | :------: | :------: | :---------: |
| `+` `-` `*` `Div` `Mod` `And` `Or` `Xor` | Integer  | Integer  |   Integer   |
| `+` `-` `*` `Div` `Mod` `And` `Or` `Xor` | Integer  | Unsigned |   Integer   |
|   `+` `*` `Div` `Mod` `And` `Or` `Xor`   | Unsigned | Unsigned |  Unsigned   |
|                   `-`                    | Unsigned | Unsigned |  Integer¹   |
|          `+` `-` `*` `/` `**`³           |   Real   | Scalar²  |    Real     |
|          `+` `-` `*` `/` `**`³           | Scalar²  |   Real   |    Real     |
|                   `+`                    |   Char   |   Char   |   String    |
|                   `+`                    |   Char   |  String  |   String    |
|                   `+`                    |  String  |   Char   |   String    |
|                   `+`                    |  String  |  String  |   String    |
|               `Shl` `Shr`                | Integer  | Integer  |   Integer   |
|               `Shl` `Shr`                | Integer  | Unsigned |   Integer   |
|               `Shl` `Shr`                | Unsigned | Integer  |  Unsigned   |
|               `Shl` `Shr`                | Unsigned | Unsigned |  Unsigned   |

¹ cf. <https://www.freepascal.org/docs-html/current/ref/refsu4.html>

² Scalar is: Integer, Unsigned, Integer subrange, Unsigned subrange, Real

³ Infix `**` is not implemented. `Power(a, b)` is available as a function, but its runtime implementation requires
Real arguments. The parser accepts other numeric arguments too, so those calls can compile and then fail at runtime.

TODO The listed `Integer`-left, `Unsigned`-right combinations are implemented. The reverse order is inconsistent: for `Unsigned`-left and `Integer`-right, `+`, `-`, `*`, `Div`, `Mod`, and `Or` infer an `Unsigned` result in the AST but produce an `Integer` value at runtime. This mismatch may cause an implicit conversion at the point of use; it does not always fail with a type error. In addition, the runtime implementation of `Integer / Real` reads the right operand as an Integer, so this documented operand pair does not compute reliably. The AST infers `Boolean` for comparisons without checking operand compatibility; unsupported operand pairs are rejected only at runtime.

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

TODO This compatibility table is not enforced as a general static rule. The compatibility helper in `ps_ast.c` has no call sites and does not implement all the listed pairs. Assignments instead attempt runtime value conversion; range checking can make otherwise convertible values fail. User-defined procedure and function arguments currently require the exact same type definition, for both by-value and by-reference parameters.
