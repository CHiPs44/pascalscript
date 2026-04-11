(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Array2;

Type
    TIndex1 = 1..10;
    NumberArray = Array[1..10] Of Integer;
    StringArray = Array[1..10] Of String;

{ Same as Free Pascal SysUtils }
Function IntToStr(N: Integer): String;
Var S: String;
    Digit: Integer;
Begin
    { Convert an Integer to a string }
    { This is a simplified version of the SysUtils.IntToStr function }
    { It does not handle all edge cases, but is sufficient for this example }
    If N = 0 Then
        S := '0'
    Else
    Begin
        S := '';
        If N < 0 Then
        Begin
            N := -N;
            S := '-';
        End;
        While N > 0 Do
        Begin
            Digit := N Mod 10;
            S := Chr(Ord('0') + Digit) + S;
            N := N Div 10;
        End;
    End;
    IntToStr := S;
End;

function Pad(N: Integer, w: Integer): String;
Var S: String;
Begin
    S := IntToStr(N);
    while Length(S) < w do
    Begin
        S := '0' + S;
    End;
    Pad := S;
End;

Var
    Numbers: NumberArray;
    Strings: StringArray;
    I, N: Integer;
    S: String;

Begin
    N := 1;
    For I := 1 To 10 Do
    Begin
        Numbers[I] := N;
        Strings[I] := {'#' + Pad(I, 2) + ' ' + }IntToStr(N);
        N := N * 2;
    End;
    WriteLn('#  | Number | String ');
    WriteLn('---|--------|--------');
    For I := 1 To 10 Do
    Begin
        S := Strings[I];
        WriteLn(I:2, ' | ', Numbers[I]:6, ' | ', S);
    End;
End.
