(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Array4;

Const
    Rows = 3;
    Cols = 5;

Type
    NumberArray = Array[1..Rows] Of Array[1..Cols] Of Integer;
    CharArray = Array[1..Rows] Of Array[1..Cols] Of Char;

Var
    Numbers: NumberArray;
    Chars: CharArray;
    I, J: Integer;

Begin
    For I := 1 To Rows Do
        For J := 1 To Cols Do
        Begin
            Numbers[I, J] := (I * J) Mod 26;
            Chars[I, J] := Chr(65 + Numbers[I, J]);
        End;
    Write('    ');
    For J := 1 To Cols Do
        Write(J:4);
    WriteLn;
    For I := 1 To Rows Do
    Begin
        Write('#', I:2, ' ');
        For J := 1 To Cols Do
            Write(Numbers[I, J]:4);
        WriteLn;
        Write('    ');
        For J := 1 To Cols Do
            Write('   ', Chars[I, J]);
        WriteLn;
    End;
End.
