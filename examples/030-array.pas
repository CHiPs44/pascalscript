(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Arrays;

Type
    NumberArray = Array[1..10] Of Integer;

Var
    Numbers: NumberArray;
    I: Integer;

Begin
    Numbers[1] := 42;
    // I := Numbers[1];
    // WriteLn(I);
    // Numbers[10] := 42;
    // I := Numbers[10];
    // WriteLn(I);
End.
