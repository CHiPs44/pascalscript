(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Arrays;

Type
    NumberArray = Array[1..10] Of Integer;

Procedure InitializeArray(Var Numbers: NumberArray);
Var
    I, N: Integer;
Begin
    WriteLn('InitializeArray');
    N := 2;
    For I := 1 To 10 Do
    Begin
        Numbers[I] := N;
        WriteLn(' - Numbers[', I:2, '] = ', Numbers[I]:4);
        N := N * 2;
    End;
End;

// #55 Passing array by value should work
Procedure PrintArray(Var Numbers: NumberArray);
Var
    I: Integer;
Begin
    WriteLn('PrintArray');
    For I := 1 To 10 Do
        WriteLn(' - Numbers[', I:2, '] = ', Numbers[I]:4);
End;

Var
    Numbers: NumberArray;

Begin
    InitializeArray(Numbers);
    PrintArray(Numbers);
End.
