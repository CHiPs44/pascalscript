<!--
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Keywords

- **STD**: ISO 7185 Standard Pascal, from <https://wiki.lazarus.freepascal.org/Standard_Pascal>
- **EXT**: ISO 10206 Extended Pascal, from <http://pascal.hansotten.com/uploads/standardpascal/iso10206_a4.pdf>
- **TP3**: Turbo Pascal 3.0, from <http://www.bitsavers.org/pdf/borland/turbo_pascal/Turbo_Pascal_Version_3.0_Reference_Manual_1986.pdf> page 37
- **TP4**: Turbo Pascal 3.0, from <http://www.bitsavers.org/pdf/borland/turbo_pascal/Turbo_Pascal_Version_4.0_Owners_Manual_1987.pdf> page 194

| Keyword          | STD | EXT | TP3 | TP4 | PS  | Notes                                 |
| ---------------- | :-: | :-: | :-: | :-: | :-: | ------------------------------------- |
| `ABSOLUTE`       |  N  |  N  |  Y  |  Y  | \*  | TODO?                                 |
| `AND_THEN`       |  N  |  Y  |  N  |  N  |  N  | "Short circuit" AND                   |
| `AND`            |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `ARRAY`          |  Y  |  Y  |  Y  |  Y  | \*  | TODO                                  |
| `BEGIN`          |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `BINDABLE`       |  N  |  Y  |  N  |  N  |  N  | ?                                     |
| `CASE`           |  Y  |  Y  |  Y  |  Y  | \*  | TODO                                  |
| `CONST`          |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `DIV`            |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `DO`             |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `DOWNTO`         |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `ELSE`           |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `END`            |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `EXPORT`         |  N  |  Y  |  N  |  N  |  N  |                                       |
| `EXTERNAL`       |  N  |  N  |  Y  |  Y  |  N  |                                       |
| `FILE`           |  Y  |  Y  |  Y  |  Y  | \*  | TODO                                  |
| `FOR`            |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `FORWARD`        |  N  |  Y  |  Y  |  Y  | \*  | TODO                                  |
| `FUNCTION`       |  Y  |  Y  |  Y  |  Y  | \*  | TODO                                  |
| `GOTO`           |  Y  |  Y  |  Y  |  Y  |  N  |                                       |
| `IF`             |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `IMPLEMENTATION` |  N  |  N  |  N  |  Y  | \*  | TODO                                  |
| `IMPORT`         |  N  |  Y  |  N  |  N  |  N  |                                       |
| `IN`             |  Y  |  Y  |  Y  |  Y  | \*  | TODO                                  |
| `INLINE`         |  N  |  N  |  Y  |  Y  |  N  |                                       |
| `INTERFACE`      |  N  |  N  |  N  |  Y  | \*  | TODO                                  |
| `INTERRUPT`      |  N  |  N  |  N  |  Y  |  N  |                                       |
| `LABEL`          |  Y  |  Y  |  Y  |  Y  |  N  |                                       |
| `MOD`            |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `MODULE`         |  N  |  Y  |  N  |  N  |  N  |                                       |
| `NIL`            |  Y  |  Y  |  Y  |  Y  | \*  | TODO                                  |
| `NOT`            |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `OF`             |  Y  |  Y  |  Y  |  Y  | \*  | TODO (for `ARRAY`,`CASE`, `SET`, ...) |
| `ONLY`           |  N  |  Y  |  N  |  N  |  N  |                                       |
| `OR_ELSE`        |  N  |  Y  |  N  |  N  |  N  | "Short circuit" OR                    |
| `OR`             |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `OTHERWISE`      |  N  |  Y  |  N  |  N  |  N  | Use `ELSE`                            |
| `OVERLAY`        |  N  |  N  |  Y  |  n  |  N  |                                       |
| `PACKED`         |  Y  |  Y  |  Y  |  Y  | \*  | TODO (ignore / skip)                  |
| `POW`            |  N  |  Y  |  N  |  N  |  N  | TP defines `**` operator              |
| `PROCEDURE`      |  Y  |  Y  |  Y  |  Y  | \*  | TODO                                  |
| `PROGRAM`        |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `QUALIFIED`      |  N  |  Y  |  N  |  N  |  N  |                                       |
| `RECORD`         |  Y  |  Y  |  Y  |  Y  | \*  | TODO                                  |
| `REPEAT`         |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `RESTRICTED`     |  N  |  Y  |  N  |  N  |  N  |                                       |
| `SET`            |  Y  |  Y  |  Y  |  Y  | \*  | TODO                                  |
| `SHL`            |  N  |  N  |  Y  |  Y  |  Y  |                                       |
| `SHR`            |  N  |  N  |  Y  |  Y  |  Y  |                                       |
| `STRING`         |  N  |  N  |  Y  |  Y  |  Y  |                                       |
| `THEN`           |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `TO`             |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `TYPE`           |  Y  |  Y  |  Y  |  Y  | \*  | TODO                                  |
| `UNIT`           |  N  |  N  |  N  |  Y  | \*  | TODO                                  |
| `UNTIL`          |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `USES`           |  N  |  N  |  N  |  Y  | \*  | TODO                                  |
| `VALUE`          |  N  |  Y  |  N  |  N  |  N  |                                       |
| `VAR`            |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `WHILE`          |  Y  |  Y  |  Y  |  Y  |  Y  |                                       |
| `WITH`           |  Y  |  Y  |  Y  |  Y  | \*  | TODO                                  |
| `XOR`            |  N  |  N  |  Y  |  Y  |  Y  |                                       |

## Turbo Pascal 3.0 "standard identifiers"

As seen on <http://www.bitsavers.org/pdf/borland/turbo_pascal/Turbo_Pascal_Version_3.0_Reference_Manual_1986.pdf> page 38.

NB:

- `Truncate`, `ParamCount`, `ParamStr`, `Abs` were added

| "Class"           | Type      | Identifier   |  PS  | Notes                                           |
| ----------------- | --------- | ------------ | :--: | ----------------------------------------------- |
| Base types        | Constant  | `False`      | Yes  |                                                 |
| Base types        | Constant  | `MaxInt`     | Yes  |                                                 |
| Base types        | Constant  | `True`       | Yes  |                                                 |
| Base types        | Type      | `Boolean`    | Yes  |                                                 |
| Base types        | Type      | `Byte`       |  No  |                                                 |
| Base types        | Type      | `Char`       | Yes  |                                                 |
| Base types        | Type      | `Integer`    | Yes  |                                                 |
| Base types        | Type      | `Real`       | Yes  |                                                 |
| I/O: Console      | Function  | `KeyPressed` |  No  |                                                 |
| I/O: Console      | Procedure | `ClrEOL`     |  No  |                                                 |
| I/O: Console      | Procedure | `ClrScr`     |  No  |                                                 |
| I/O: Console      | Procedure | `CrtExit`    |  No  |                                                 |
| I/O: Console      | Procedure | `CrtInit`    |  No  |                                                 |
| I/O: Console      | Procedure | `DelLine`    |  No  |                                                 |
| I/O: Console      | Procedure | `GotoXY`     |  No  |                                                 |
| I/O: Console      | Procedure | `InsLine`    |  No  |                                                 |
| I/O: Console      | Procedure | `LowVideo`   |  No  |                                                 |
| I/O: Console      | Procedure | `NormVideo`  |  No  |                                                 |
| I/O: File         | Function  | `EOF`        | TODO |                                                 |
| I/O: File         | Function  | `EOLN`       | TODO |                                                 |
| I/O: File         | Function  | `FilePos`    | TODO |                                                 |
| I/O: File         | Function  | `FileSize`   | TODO |                                                 |
| I/O: File         | Function  | `SeekEof`    | TODO |                                                 |
| I/O: File         | Function  | `SeekEoln`   | TODO |                                                 |
| I/O: File         | Procedure | `Assign`     | TODO |                                                 |
| I/O: File         | Procedure | `BlockRead`  | TODO |                                                 |
| I/O: File         | Procedure | `BlockWrite` | TODO |                                                 |
| I/O: File         | Procedure | `Close`      | TODO |                                                 |
| I/O: File         | Procedure | `Erase`      | TODO |                                                 |
| I/O: File         | Procedure | `Flush`      | TODO |                                                 |
| I/O: File         | Procedure | `Read`       | TODO | Special parameters                              |
| I/O: File         | Procedure | `ReadLn`     | TODO | Special parameters                              |
| I/O: File         | Procedure | `Rename`     | TODO |                                                 |
| I/O: File         | Procedure | `Reset`      | TODO |                                                 |
| I/O: File         | Procedure | `Rewrite`    | TODO |                                                 |
| I/O: File         | Procedure | `Seek`       | TODO |                                                 |
| I/O: File         | Procedure | `Truncate`   | TODO |                                                 |
| I/O: File         | Procedure | `Write`      | Yes  | Special parameters                              |
| I/O: File         | Procedure | `WriteLn`    | Yes  | Special parameters                              |
| I/O: File         | Type      | `Text`       | TODO |                                                 |
| I/O: File         | Variable  | `BufLen`     |  No  |                                                 |
| I/O: File         | Variable  | `Input`      | TODO |                                                 |
| I/O: File         | Variable  | `IOresult`   | TODO |                                                 |
| I/O: File         | Variable  | `Output`     | TODO |                                                 |
| I/O: Devices      | Variable  | `Aux`        |  No  | `AUX:`                                          |
| I/O: Devices      | Variable  | `AuxInptr`   |  No  |                                                 |
| I/O: Devices      | Variable  | `AuxOutPtr`  |  No  |                                                 |
| I/O: Devices      | Variable  | `Con`        |  No  | `CON:`                                          |
| I/O: Devices      | Variable  | `ConInPtr`   |  No  |                                                 |
| I/O: Devices      | Variable  | `ConOutPtr`  |  No  |                                                 |
| I/O: Devices      | Variable  | `ConStPtr`   |  No  |                                                 |
| I/O: Devices      | Variable  | `Kbd`        |  No  | `KBD:` Input only                               |
| I/O: Devices      | Variable  | `Lst`        |  No  | `LST:`                                          |
| I/O: Devices      | Variable  | `LstOutPtr`  |  No  |                                                 |
| I/O: Devices      | Variable  | `Trm`        |  No  | `TRM:`                                          |
| I/O: Devices      | Variable  | `Usr`        |  No  | `USR:`                                          |
| I/O: Devices      | Variable  | `UsrInptr`   |  No  |                                                 |
| I/O: Devices      | Variable  | `UsrOutPtr`  |  No  |                                                 |
| Math / arithmetic | Constant? | `Pi`         | Yes  |                                                 |
| Math / arithmetic | Function  | `Abs`        | Yes  |                                                 |
| Math / arithmetic | Function  | `ArcTan`     | Yes  |                                                 |
| Math / arithmetic | Function  | `Cos`        | Yes  |                                                 |
| Math / arithmetic | Function  | `Exp`        | Yes  |                                                 |
| Math / arithmetic | Function  | `Frac`       | Yes? |                                                 |
| Math / arithmetic | Function  | `Int`        | Yes  |                                                 |
| Math / arithmetic | Function  | `Ln`         | Yes  |                                                 |
| Math / arithmetic | Function  | `Log`        | Yes  |                                                 |
| Math / arithmetic | Function  | `Odd`        | Yes  |                                                 |
| Math / arithmetic | Function  | `Random`     | Yes  |                                                 |
| Math / arithmetic | Function  | `Round`      | Yes  |                                                 |
| Math / arithmetic | Function  | `Sin`        | Yes  |                                                 |
| Math / arithmetic | Function  | `Sqr`        | Yes  |                                                 |
| Math / arithmetic | Function  | `Sqrt`       | Yes  |                                                 |
| Math / arithmetic | Function  | `Trunc`      | Yes  |                                                 |
| Math / arithmetic | Procedure | `Randomize`  | Yes  |                                                 |
| Math / scalars    | Function  | `Chr`        | Yes  |                                                 |
| Math / scalars    | Function  | `Ord`        | TODO |                                                 |
| Math / scalars    | Function  | `Pred`       | Yes  |                                                 |
| Math / scalars    | Function  | `Succ`       | Yes  |                                                 |
| Memory / Heap     | Function  | `MemAvail`   | TODO |                                                 |
| Memory / Heap     | Function  | `New`        | TODO |                                                 |
| Memory / Heap     | Procedure | `GetMem`     | TODO |                                                 |
| Memory / Heap     | Procedure | `Mark`       | TODO |                                                 |
| Memory / Heap     | Procedure | `Release`    | TODO |                                                 |
| Memory / Heap     | Variable  | `HeapPtr`    | TODO |                                                 |
| Strings           | Function  | `Concat`     | TODO |                                                 |
| Strings           | Function  | `Copy`       | Yes? |                                                 |
| Strings           | Function  | `Length`     | Yes? |                                                 |
| Strings           | Function  | `Pos`        | Yes? |                                                 |
| Strings           | Function  | `UpCase`     | TODO |                                                 |
| Strings           | Procedure | `Delete`     | Yes? |                                                 |
| Strings           | Procedure | `Insert`     | TODO |                                                 |
| Strings           | Procedure | `Str`        | TODO |                                                 |
| Strings           | Procedure | `Val`        | TODO |                                                 |
| System            | Function  | `Addr`       |  No  |                                                 |
| System            | Function  | `Hi`         |  No  | High byte of 16 bits word                       |
| System            | Function  | `Lo`         |  No  | Low byte of 16 bits word                        |
| System            | Function  | `Ofs`        |  No  | x86: offset part of address                     |
| System            | Function  | `ParamCount` | TODO | Argument count for CLI                          |
| System            | Function  | `ParamStr`   | TODO | Argument values for CLI                         |
| System            | Function  | `Ptr`        |  No  | From 16 bits address or segment + offset        |
| System            | Function  | `Seg`        |  No  | x86: segment part of address                    |
| System            | Function  | `SizeOf`     | TODO | in bytes                                        |
| System            | Procedure | `Chain`      |  No  |                                                 |
| System            | Procedure | `Delay`      | TODO | in approx. ms                                   |
| System            | Procedure | `Execute`    |  No  |                                                 |
| System            | Procedure | `Exit`       | TODO |                                                 |
| System            | Procedure | `FillChar`   | TODO | C `memset` equivalent                           |
| System            | Procedure | `Halt`       | TODO |                                                 |
| System            | Procedure | `Move`       | TODO | C `memcpy` equivalent (with overlap management) |
| System            | Procedure | `Swap`       |  No  | The 2 8 bits bytes of 16 bits word              |
| System            | Variable  | `Mem`        |  No  | Virtual array for byte memory access            |
| System            | Variable  | `MemW`       |  No  | Virtual array for word memory access            |
| System            | Variable  | `Port`       |  No  | Virtual array for byte memory access            |
| System            | Variable  | `PortW`      |  No  | Virtual array for word memory access            |
