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
    C: Char;
    N: Integer;

Begin
    N := 10;
    For C := 'A' To 'F' Do
    Begin
        Numbers[C] := N;
        N := N + 1;
    End;
    For C := 'A' To 'F' Do
        WriteLn('''', C:2, ''' = ', Numbers[C]:3);
End.
