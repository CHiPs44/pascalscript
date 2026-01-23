(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program RepeatUntilReal;
Const
    Limit = 1.0;
Var
    R : Real;
    Z : Real;
Begin
    WriteLn('Example 045: Repeat-Until Loop with Real numbers');
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('         1         2         3         4         5         6         7         8');
    WriteLn('12345678901234567890123456789012345678901234567890123456789012345678901234567890');
    R := -Limit;
    Repeat
        Z := R * 10.0;
        Write(Z:10:1);
        If (Z>=-3.0 And Z<-2.0) Or (Z>=5.0 And Z<6.0) Then
            WriteLn;
        R := R + 0.1;
    Until R - 0.01 > Limit;
    WriteLn;
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('OK!');
End.
