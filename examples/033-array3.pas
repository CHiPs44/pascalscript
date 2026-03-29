(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Arrays;

Type
    ArraySubRange = -9..9;
    NumberArray = Array[ArraySubRange] Of Integer;

Procedure InitializeArray(Var Numbers: NumberArray);
Var
    I: Integer;
Begin
    WriteLn('InitializeArray');
    Randomize;
    For I := Low(Numbers) To High(Numbers) Do
        Numbers[I] := 100 + Random(900);
End;

Procedure SortArray(Var Numbers: NumberArray);
Var
    I, J, Temp: Integer;
Begin
    WriteLn('SortArray');

    // Bubble sort algorithm
    For I := Low(Numbers) To High(Numbers) Do
    Begin
        For J := I + 1 To High(Numbers) Do
        Begin
            If Numbers[I] > Numbers[J] Then
            Begin
                // Swap elements
                Temp := Numbers[I];
                Numbers[I] := Numbers[J];
                Numbers[J] := Temp;
            End;
        End;
    End;
End;

// #55 Passing array by value should work
Procedure PrintArray(Var Numbers: NumberArray);
Var
    I: Integer;
Begin
    WriteLn('PrintArray');
    For I := Low(Numbers) To High(Numbers) Do
        WriteLn(' - Numbers[', I:2, '] = ', Numbers[I]:4);
End;

Var
    Numbers: NumberArray;
    L, H: Integer;

Begin
    L := Low(ArraySubRange);
    H := High(ArraySubRange);
    WriteLn('ArraySubRange: ', L,' ', H);
    L := Low(NumberArray);
    H := High(NumberArray);
    WriteLn('  NumberArray: ', L,' ', H);
    L := Low(Numbers);
    H := High(Numbers);
    WriteLn('      Numbers: ', L,' ', H);
    InitializeArray(Numbers);
    PrintArray(Numbers);
    SortArray(Numbers);
    PrintArray(Numbers);
End.
