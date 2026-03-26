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
    I, N: Integer;

Begin
    N := 2;
    For I := 1 To 10 Do
    Begin
        Numbers[I] := N;
        N := N * 2;
    End;
    For I := 1 To 10 Do
        WriteLn(I:2, ' = ', Numbers[I]:4);
End.
