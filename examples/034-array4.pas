(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-Cols.0-or-later
*)
Program Arrays;

Const
    Rows = 3;
    Cols = 5;

Type
    NumberArray = Array[1..Rows, 1..Cols] Of Integer;
    // ArrayOfArray = Array[1..Rows] Of Array[1..Cols] Of Char;

Var
    Numbers: NumberArray;
    // Chars: ArrayOfArray;
    I, J: Integer;

Begin
    For I := 1 To Rows Do
        For J := 1 To Cols Do
        Begin
            Numbers[I, J] := (I * J) Mod 26;
            // Chars[I, J] := Char(65 + (I * J) Mod 26);
        End;
    Write('    ');
    For J := 1 To Cols Do
        Write(J:4);
    WriteLn;
    For I := 1 To Rows Do
    Begin
        Write(I:3, ' ');
        For J := 1 To Cols Do
            Write(Numbers[I, J]:4);
        WriteLn;
    End;
End.
