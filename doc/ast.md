# AST - Abstract Syntax Tree

As of 2026-05-21, use of a compiler generating an AST then an interpreter executing this AST is on its way.

Direct interpretation within the recursive descent parser was really too cumbersome to implement, mostly with array item access...

Running a program now has 2 phases:

1. **Compilation** - Parsing the source file and building the AST
2. **Interpretation** - Executing the AST

NB:

- The AST is built in memory, no file is generated
- The AST is not optimized, it is interpreted as-is
- Many features of previous design were kept, hence the `ps_value` / `ps_ast_value` existence

This may lead to other steps:

- emitting "P-Code" from the AST for a stack-based virtual machine
- then interpreting this P-Code like Pascal P4

## Implementation of AST nodes

As C has no classes, the AST nodes are implemented as structs with common fields at the beginning.

These common fields are:

- Line & column in source file
- Group: Block, Statement, Expression, LValue
- Kind:
  - Block: Program, Procedure, Function, Unit (future)
  - Statement: Statement list (compound), Assignment, If, Repeat, While, For, Procedure call
  - Expression: Constant, Variable, Function Call, Unary, Binary
  - LValue: Variable

This means we have to cast the AST node to the proper type to access its specific fields and vice-versa.

This also means these structs can not be packed, as the size of the common fields must be the same for all nodes.

## Interpretation: Stack & frames

Interpreter uses a stack of frames to manage variables values and function calls.

When a procedure or function is called, a new frame is pushed on the stack for the parameters and local variables.

Variables are kept in the local symbol table they are defined in, their values are stored in a frame and accessed through an handle corresponding to their order of declaration.

Global variables are accessed

Functions automagically reserve a `Result` variable.
