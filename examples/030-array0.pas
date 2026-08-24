(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Array0;

Type
    MySubRange = 1..10;
    MyInteger = Integer;
    NumberArray = Array[MySubRange] Of MyInteger;

Var
    I, N: Integer;
    Numbers: NumberArray;

Begin
    N := Numbers[1];
    // Numbers[1] := 42;
    // N := 1;
    // For I := 1 To 10 Do
    // Begin
    //     Numbers[I] := N;
    //     N := N * 2;
    // End;
    // For I := 1 To 10 Do
    //     WriteLn('Numbers[', I:2, '] = ', Numbers[I]:4);
End.
