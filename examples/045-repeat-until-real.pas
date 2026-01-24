(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program RepeatUntilReal;
Const
    Limit = 1.0;
Var
    C : Integer;
    R : Real;
Begin
    WriteLn('Example 045: Repeat-Until Loop with Real numbers');
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('         1         2         3         4         5         6         7         8');
    WriteLn('12345678901234567890123456789012345678901234567890123456789012345678901234567890');
    R := -Limit;
    C := 1;
    Repeat
        Write(R:10:6);
        C := C + 10;
        If C > 80 Then
        Begin
            C := 1;
            WriteLn;
        End;
        R := R + Limit / 10.0;
    Until R - EpsReal > Limit;
    WriteLn;
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('OK!');
End.
