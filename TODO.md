# TODO

Created 2026-10-15.

## `PS_ERROR_NOT_IMPLEMENTED` follow-up

Review these locations to determine which unsupported cases should be implemented. There are 21 textual matches; two are commented-out code and inactive. The table also includes the related `byref` argument TODO in the selected call-execution file.

| File                                                            |             Lines | Follow-up                                                                              |
| --------------------------------------------------------------- | ----------------: | -------------------------------------------------------------------------------------- |
| [`include/ps_error.h`](include/ps_error.h#L24)                  |                24 | Error-code declaration; retain while callers still report unimplemented features.      |
| [`src/ps_error.c`](src/ps_error.c#L24)                          |                24 | Error-code display text; retain while the error code is in use.                        |
| [`src/ps_execute_call.c`](src/ps_execute_call.c#L39)            |                39 | Implement execution of user-defined function calls.                                    |
| [`src/ps_execute_call.c`](src/ps_execute_call.c#L72)            |                72 | Initialize and pass `byref` procedure arguments correctly.                             |
| [`src/ps_execute_statement.c`](src/ps_execute_statement.c#L76)  |                76 | Implement the statement case that currently reports this error.                        |
| [`src/ps_execute_system.c`](src/ps_execute_system.c#L126)       |               126 | Implement the unsupported system operation.                                            |
| [`src/ps_functions.c`](src/ps_functions.c#L119)                 |     119, 135, 153 | Implement the three functions that return this error.                                  |
| [`src/ps_interpreter.c`](src/ps_interpreter.c#L254)             |               254 | Replace the `TODO!` interpreter path with its implementation.                          |
| [`src/ps_parse.c`](src/ps_parse.c#L36)                          |                36 | Implement the unsupported parser case.                                                 |
| [`src/ps_parse_call.c`](src/ps_parse_call.c#L360)               |          360, 374 | Implement the unsupported call-parsing cases.                                          |
| [`src/ps_parse_declaration.c`](src/ps_parse_declaration.c#L203) |          203, 208 | Decide whether the commented-out error returns are still relevant; these are inactive. |
| [`src/ps_parse_expression.c`](src/ps_parse_expression.c#L494)   |               494 | Implement the unsupported expression case.                                             |
| [`src/ps_parse_statement.c`](src/ps_parse_statement.c#L160)     |               160 | Implement the unsupported statement-parsing case.                                      |
| [`src/ps_parse_type.c`](src/ps_parse_type.c#L697)               |               697 | Implement the unsupported type-parsing case.                                           |
| [`src/ps_procedures.c`](src/ps_procedures.c#L59)                | 59, 139, 149, 159 | Implement `INC`/`DEC`, `READ`, `READLN`, and `READSTR`.                                |

## Other TODOs

| File                                                               | Line | Follow-up                                                                                                          |
| ------------------------------------------------------------------ | ---: | ------------------------------------------------------------------------------------------------------------------ |
| [`src/ps_parse_assignment.c`](src/ps_parse_assignment.c#L148)      |  148 | Check that an assignment's rvalue type matches its lvalue type.                                                    |
| [`src/ps_parse_type.c`](src/ps_parse_type.c#L250)                  |  250 | Determine whether the type symbol must be removed from the symbol table on parse cleanup, and handle it if needed. |
| [`src/ps_value.c`](src/ps_value.c#L250)                            |  250 | Handle string and array copying correctly in `ps_value_copy`.                                                      |
| [`src/ps_value.c`](src/ps_value.c#L461)                            |  461 | Use a handle when formatting strings held by variables in an upper-level scope.                                    |
| [`include/ps_type_definition.h`](include/ps_type_definition.h#L40) |   40 | Revisit enum value storage; the comment proposes a variable-length array of symbols.                               |
