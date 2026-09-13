# System library

This document describes the system procedures and functions currently registered by the engine.

Note:

- `Number` stands for `Integer`, `Unsigned` or `Real`
- `Ordinal` stands for `Integer`, `Unsigned`, `Char`,`Boolean`, `Subrange` or `Enum`

## Input/Output

These are the two canonical procedures with a variadic number of parameters, which have to be implemented at the language level.

Implementation is made with a single parameter and looped at execution.

| Definition                              | Purpose                                                                                                        |
| --------------------------------------- | -------------------------------------------------------------------------------------------------------------- |
| `Write([Value[:Width[:Precision]]]*)`   | Writes values to standard output, optionally with a field width and precision.                                 |
| `WriteLn([Value[:Width[:Precision]]]*)` | Writes values to standard output and then moves to the next line, optionally with a field width and precision. |
| `Read(Value)`                           | Reads a value from standard input.                                                                             |
| `ReadLn(Value)`                         | Reads a value from standard input and advances to the next line.                                               |

Up to 16 (`PS_PARAMETERS_MAX`) parameters can be passed to `Write` and `WriteLn`.

## Math

| Family        | Definition                                | Purpose                                                                          |
| ------------- | ----------------------------------------- | -------------------------------------------------------------------------------- |
| Constant      | `EpsReal`: `Real`                         | Smallest positive real number such that `1.0 + EpsReal > 1.0`.                   |
| Constant      | `MaxReal`: `Real`                         | Maximum value of a real number.                                                  |
| Constant      | `MinReal`: `Real`                         | Minimum value of a real number.                                                  |
| Constant      | `Pi`: `Real`                              | Mathematical constant for the ratio of a circle's circumference to its diameter. |
| Exponential   | `Exp(X: Real): Real`                      | Returns the exponential function `e^X`.                                          |
| Exponential   | `Ln(X: Real): Real`                       | Returns the natural logarithm of `X`.                                            |
| Exponential   | `Log(X: Real): Real`                      | Returns the logarithm of `X` (base 10 in Pascal-style usage).                    |
| Exponential   | `Power(Base: Real; Exponent: Real): Real` | Raises `Base` to the power `Exponent`.                                           |
| Numeric       | `Abs(X: Number): Number`                  | Returns the absolute value of a number.                                          |
| Numeric       | `Frac(X: Real): Real`                     | Returns the fractional part of `X`.                                              |
| Numeric       | `Int(X: Real): Real`                      | Returns the integer part of `X` truncated toward zero.                           |
| Numeric       | `Round(X: Real): Integer`                 | Rounds `X` to the nearest integer.                                               |
| Numeric       | `Sqr(X: Real): Real`                      | Returns the square of `X`.                                                       |
| Numeric       | `Sqrt(X: Real): Real`                     | Returns the square root of `X`.                                                  |
| Numeric       | `Trunc(X: Real): Integer`                 | Returns the integer part of `X` by truncating toward zero.                       |
| Predicate     | `Even(X: Ordinal): Boolean`               | Tests whether an ordinal value is even.                                          |
| Predicate     | `Odd(X: Ordinal): Boolean`                | Tests whether an ordinal value is odd.                                           |
| Trigonometric | `Arctan(X: Real): Real`                   | Returns the angle whose tangent is `X` in radians.                               |
| Trigonometric | `Cos(X: Real): Real`                      | Returns the cosine of an angle `X` in radians.                                   |
| Trigonometric | `Sin(X: Real): Real`                      | Returns the sine of an angle `X` in radians.                                     |
| Trigonometric | `Tan(X: Real): Real`                      | Returns the tangent of an angle `X` in radians.                                  |

Note:

- `Log` is the same as `Log10` in Pascal.
- `Power` is the same as `**` in Pascal.

## Random

| Definition                                                        | Purpose                                                                                                          |
| ----------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------- |
| `Random([Max: Integer \| Unsigned]): Real \| Integer \| Unsigned` | Returns a random value in `[0.0, 1.0[` when called without an argument, or in `[0, Max[` when `Max` is provided. |
| `Randomize([Seed: Integer \| Unsigned])`                          | Initializes the random number generator with an optional seed.                                                   |

## Time

| Definition                 | Purpose                                                                         |
| -------------------------- | ------------------------------------------------------------------------------- |
| `GetTickCount(): Unsigned` | Returns the number of milliseconds that have elapsed since the program started. |

## String

| Definition | Purpose |
program ------------------------------ | --------------------------------------------- |
| `Length(S: String): Unsigned` | Returns the number of characters in `S`. |
| `LowerCase(S: String): String` | Returns a copy of `S` converted to lowercase. |
| `UpperCase(S: String): String` | Returns a copy of `S` converted to uppercase. |

## Conversion

| Definition                  | Purpose                                                           |
| --------------------------- | ----------------------------------------------------------------- |
| `Chr(X: Ordinal): Char`     | Returns the character corresponding to the given ordinal value.   |
| `Ord(X: Ordinal): Unsigned` | Returns the ordinal value of the given ordinal type or character. |

## Other

| Definition                         | Purpose                                                              |
| ---------------------------------- | -------------------------------------------------------------------- |
| `Low(Type \| Variable): Ordinal`   | Returns the lowest valid value for a type or variable.               |
| `High(Type \| Variable): Ordinal`  | Returns the highest valid value for a type or variable.              |
| `Pred(X: Ordinal): same type as X` | Returns the previous value before `X` in its ordinal type.           |
| `Succ(X: Ordinal): same type as X` | Returns the next value after `X` in its ordinal type.                |
| `Inc(Variable)`                    | Increments a variable by one. This procedure is not implemented yet. |
| `Dec(Variable)`                    | Decrements a variable by one. This procedure is not implemented yet. |
