(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example002TestExpr1;
Var
    I, J: Integer;
    R: Real;
Begin
    I := 1 + 2 * 3 - 4; // Should evaluate to 3
    J := (1 + 2) div (3 - 4); // Should evaluate to -3
    R := 10.0 / 4.0; // Should evaluate to 2.5
    // R := Cos(0.0); // Should evaluate to 1.0
    // WriteLn(I);
    // WriteLn(J);
    // WriteLn(R);
End.
