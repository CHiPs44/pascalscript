# System library

This document lists the symbols currently added to the runtime system block by `ps_system_alloc()`. Pascal identifiers are case-insensitive; names below are shown in Pascal style.

Note:

- `Number` stands for `Integer`, `Unsigned` or `Real`
- `Ordinal` stands for `Integer`, `Unsigned`, `Char`, `Boolean`, `Subrange` or `Enum`

## Built-in constants

| Definition                        | Type       | Use                            |
| --------------------------------- | ---------- | ------------------------------ |
| `False` / `True`                  | `Boolean`  | Logical constants              |
| `MaxInt` / `MinInt`               | `Integer`  | Integer bounds                 |
| `MaxUInt`                         | `Unsigned` | Maximum unsigned integer value |
| `MaxReal` / `MinReal` / `EpsReal` | `Real`     | Real bounds and epsilon        |
| `Pi`                              | `Real`     | Mathematical constant π        |
| `PS_BITNESS`                      | `Unsigned` | Runtime bitness (`32` or `64`) |
| `PS_VERSION`                      | `String`   | PascalScript version string    |

## Built-in variables

| Definition | Type      | Use                                  |
| ---------- | --------- | ------------------------------------ |
| `IOResult` | `Integer` | Standard runtime I/O status variable |
| `ExitCode` | `Integer` | Standard runtime exit code           |

## Input / output

| Definition                                          | Status                                      | Use                                           |
| --------------------------------------------------- | ------------------------------------------- | --------------------------------------------- |
| `Write([Value[:Width[:Precision]]]*)`               | Implemented                                 | Writes values to standard output              |
| `WriteLn([Value[:Width[:Precision]]]*)`             | Implemented                                 | Writes values and then moves to the next line |
| `WriteStr(StringVar; [Value[:Width[:Precision]]]*)` | Implemented                                 | Appends formatted text to a string variable   |
| `Read(Value)`                                       | Registered, not implemented                 | Reads a value from standard input             |
| `ReadLn(Value)`                                     | Registered, not implemented                 | Reads a value and advances to next line       |
| `ReadStr(StringVar, Value)`                         | Parser supports it, runtime not implemented | Reads from a string buffer into a variable    |

Up to 16 (`PS_PARAMETERS_MAX`) parameters can be passed to `Write`, `WriteLn` and `WriteStr`

This includes the `StringVar` parameter, and the `File` parameter when file I/O will be implemented.

## Math

| Family        | Definition                                | Use                                                           |
| ------------- | ----------------------------------------- | ------------------------------------------------------------- |
| Constant      | `EpsReal: Real`                           | Smallest positive real number such that `1.0 + EpsReal > 1.0` |
| Constant      | `MaxReal: Real`                           | Maximum value of a real number                                |
| Constant      | `MinReal: Real`                           | Minimum value of a real number                                |
| Constant      | `Pi: Real`                                | Ratio of a circle's circumference to its diameter             |
| Exponential   | `Exp(X: Real): Real`                      | Returns `e^X`                                                 |
| Exponential   | `Ln(X: Real): Real`                       | Returns the natural logarithm of `X`                          |
| Exponential   | `Log10(X: Real): Real`                    | Returns the base-10 logarithm                                 |
| Exponential   | `Log2(X: Real): Real`                     | Returns the base-2 logarithm                                  |
| Exponential   | `LogN(Base: Real; X: Real): Real`         | Returns the logarithm of `X` in base `Base`                   |
| Exponential   | `Power(Base: Real; Exponent: Real): Real` | Raises `Base` to the power `Exponent`                         |
| Numeric       | `Abs(X: Number): Number`                  | Absolute value                                                |
| Numeric       | `Frac(X: Real): Real`                     | Fractional part                                               |
| Numeric       | `Int(X: Real): Real`                      | Integer part (truncated toward zero)                          |
| Numeric       | `Round(X: Real): Integer`                 | Rounds to nearest integer                                     |
| Numeric       | `Sqr(X: Real): Real`                      | Square                                                        |
| Numeric       | `Sqrt(X: Real): Real`                     | Square root                                                   |
| Numeric       | `Trunc(X: Real): Integer`                 | Truncates toward zero                                         |
| Predicate     | `Even(X: Ordinal): Boolean`               | `True` when `X` is even                                       |
| Predicate     | `Odd(X: Ordinal): Boolean`                | `True` when `X` is odd                                        |
| Trigonometric | `Arctan(X: Real): Real`                   | Arc tangent                                                   |
| Trigonometric | `Cos(X: Real): Real`                      | Cosine                                                        |
| Trigonometric | `Sin(X: Real): Real`                      | Sine                                                          |
| Trigonometric | `Tan(X: Real): Real`                      | Tangent                                                       |

Note:

- The runtime exposes `Log10`, `Log2` and `LogN` as distinct functions. Pascal-style `Log` is therefore represented by these system symbols rather than a single `Log` symbol.
- `Power` is the same as the `**` operator introduced by Free Pascal.

## Random

| Definition                                              | Use                                                           |
| ------------------------------------------------------- | ------------------------------------------------------------- |
| `Random(): Real`                                        | Returns a random value in `[0.0, 1.0[`                        |
| `Random(Max: Integer \| Unsigned): Integer \| Unsigned` | Returns a random value in `[0, Max[` when `Max` is provided   |
| `Randomize([Seed: Integer \| Unsigned])`                | Initializes the random number generator with an optional seed |

## Time

| Definition                 | Use                                                            |
| -------------------------- | -------------------------------------------------------------- |
| `GetTickCount(): Unsigned` | Returns the number of milliseconds elapsed since program start |

## String

| Definition                     | Use                                     |
| ------------------------------ | --------------------------------------- |
| `Length(S: String): Unsigned`  | Returns the number of characters in `S` |
| `LowerCase(S: String): String` | Returns a lowercase copy of `S`         |
| `UpperCase(S: String): String` | Returns an uppercase copy of `S`        |

## Conversion

| Definition                  | Use                                                      |
| --------------------------- | -------------------------------------------------------- |
| `Chr(X: Ordinal): Char`     | Returns the character corresponding to the ordinal value |
| `Ord(X: Ordinal): Unsigned` | Returns the ordinal value of the given type or character |

## Other

| Definition                        | Use                                                                                |
| --------------------------------- | ---------------------------------------------------------------------------------- |
| `Low(Type \| Variable): Ordinal`  | Lowest value of a type or variable, or lowest index of an array type or variable   |
| `High(Type \| Variable): Ordinal` | Highest value of a type or variable, or highest index of an array type or variable |
| `Pred(X: Ordinal): Ordinal`       | Predecessor: value before `X`                                                      |
| `Succ(X: Ordinal): Ordinal`       | Successor: value after `X`                                                         |
| `Inc(Variable [ , Amount ])`      | Registered, but not implemented yet                                                |
| `Dec(Variable [ , Amount ])`      | Registered, but not implemented yet                                                |

## Implementation status

The current engine registers the following runtime entries:

- implemented: `Write`, `WriteLn`, `WriteStr`, `Randomize`, `Random`, `GetTickCount`, all math functions listed above, `Chr`, `Ord`, `Low`, `High`, `Pred`, `Succ`, `Length`, `LowerCase`, `UpperCase`
- registered but intentionally NOT implemented yet: `Read`, `ReadLn`, `ReadStr`, `Inc`, `Dec`

This matches the runtime code in `src/ps_system.c`, `src/ps_procedures.c`, and `src/ps_functions.c`.
