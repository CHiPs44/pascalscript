(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program TestExpression1;
Var
    U: Unsigned;
    I: Integer;
    R: Real;
Begin
    WriteLn('Example 002 - Test expression #1');
    WriteLn('--------------------------------');
    U := 1 + (2 * 3) - 4;     // Should evaluate to 3
    I := (1 + 2) div (4 - 3); // Should evaluate to 3
    R := 12.34 + 10.0 / 4.0;  // Should evaluate to 14.84
    WriteLn('U=', U:5, '    (delta=', U - 3, ')');
    WriteLn('I=', I:5, '    (delta=', I - 3, ')');
    WriteLn('R=', R:8:2, ' (delta=', (R - 14.84):8:7, ')');
End.
