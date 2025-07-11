(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example04RepeatUntilReal;
Const
    Limit = 1.0;
Var
    R : Real;
    Z : Real;
Begin
    R := -Limit;
    Repeat
        Z := R * 10.0;
        Write(Z, ' ');
        R := R + 0.1;
    Until R - 0.01 > Limit;
    WriteLn('O', 'K', '!');
End.
