(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program RepeatUntil;
Const
    Limit = 100000000;
Var
    C: Integer;
    I: Integer;
Begin
    WriteLn('Example 040: Repeat-Until Loop with Integer numbers');
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('         1         2         3         4         5         6         7         8');
    WriteLn('12345678901234567890123456789012345678901234567890123456789012345678901234567890');
    WriteLn('--------------------------------------------------------------------------------');
    I := -Limit;
    C := 1;
    Repeat
        Write(I:10);
        C := C + 10;
        If C > 80 Then
        Begin
            C := 1;
            WriteLn;
        End;
        I := I + 10000000;
    Until I > Limit;
    WriteLn;
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('OK!');
End.
