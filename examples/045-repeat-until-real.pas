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
    WriteLn('Example 04: Repeat-Until Loop with Real numbers');
    WriteLn('--------------------------------------------------------------------------------');
    R := -Limit;
    Repeat
        Z := R * 10.0;
        WriteLn(Z);
        R := R + 0.1;
    Until R - 0.01 > Limit;
    WriteLn('O', 'K', '!');
    WriteLn('--------------------------------------------------------------------------------');
End.
