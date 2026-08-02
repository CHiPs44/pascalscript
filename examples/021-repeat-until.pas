(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program RepeatUntil;

Const
    Limit = 100000000;
    Delta = 10000000;

Var
    Column: Integer;
    Value: Integer;

Begin
    WriteLn('Example 040: Repeat-Until Loop with Integer numbers');
    WriteLn('--------------------------------------------------------------------------------');
    Value := -Limit;
    Column := 1;
    Repeat
        Write(Value:10);
        Column := Column + 10;
        If Column > 80 Then
        Begin
            Column := 1;
            WriteLn;
        End;
        Value := Value + Delta;
    Until Value > Limit;
    WriteLn;
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('OK!');
End.
