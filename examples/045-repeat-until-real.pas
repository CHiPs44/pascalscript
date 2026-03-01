(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program RepeatUntilReal;

Const
    Limit = 1.0;
    Delta = 0.1;

Var
    Column : Integer;
    Value : Real;

Begin
    WriteLn('Example 045: Repeat-Until Loop with Real numbers');
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('         1         2         3         4         5         6         7         8');
    WriteLn('12345678901234567890123456789012345678901234567890123456789012345678901234567890');
    WriteLn('--------------------------------------------------------------------------------');
    Value := -Limit;
    Column := 1;
    Repeat
        Write(Value:10:6);
        Column := Column + 10;
        If Column > 80 Then
        Begin
            Column := 1;
            WriteLn;
        End;
        Value := Value + Delta;
    Until Value - EpsReal > Limit;
    WriteLn;
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('OK!');
End.
