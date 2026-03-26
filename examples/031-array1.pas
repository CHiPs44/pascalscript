(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Arrays;

Type
    NumberArray = Array['A'..'F'] Of Integer;

Var
    Numbers: NumberArray;
    // I: Integer;
    I: Char;
    N: Integer;

Begin
    // For I := 1 To 10 Do
    N := 10;
    For I := 'A' To 'F' Do
    Begin
        Numbers[I] := N;
        N := N + 1;
    End;
    // For I := 1 To 10 Do
    For I := 'A' To 'F' Do
        WriteLn('''', I:2, ''' = ', Numbers[I]:3);
End.
