(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program RepeatUntil;
Const
    Loops = 10;
Var
    I : Integer;
Begin
    WriteLn('Example 040: Repeat-Until Loop with Integer numbers');
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('         1         2         3         4         5         6         7         8');
    WriteLn('12345678901234567890123456789012345678901234567890123456789012345678901234567890');
    I := -Loops;
    Repeat
        Write(I:10);
        I := I + 1;
    Until I > Loops;
    WriteLn;
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('OK!');
End.
