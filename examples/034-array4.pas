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
    // NumberArray   = Array[1..Rows] Of Array[1..Cols] Of Integer;
    NumberArray   = Array[1..Rows, 1..Cols] Of Integer;
    // CharArray     = Array[1..Rows] Of Array[1..Cols] Of Char;
    CharArray     = Array[1..Rows, 1..Cols] Of Char;
    ArrayOfArray2 = Array[1..2] Of CharArray;
    Array3D       = Array[1..10, 1..10, 1..10] Of Integer;

// Function GetPoint(Var A: Array3D; X, Y, Z: Integer): Integer;
// Begin
//     Result := A[X, Y, Z];
// End;

Var
    Numbers: NumberArray;
    Chars: CharArray;
    I, J, K: Integer;
    A: Array3D;

Begin
    WriteLn('Array4');
    // A[1, 2, 3] := 42;
    // I := A[1, 2, 3];
    // WriteLn(A[1, 2, 3]);
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
    For I := 1 To 10 Do
        For J := 1 To 10 Do
            For K := 1 To 10 Do
                A[I, J, K] := I * J * K;
    For I := 1 To 10 Do
        For J := 1 To 10 Do
            For K := 1 To 10 Do
                WriteLn('A[', I:2, ', ', J:2, ', ', K:2 , '] = ', A[I, J, K]:4);
End.
