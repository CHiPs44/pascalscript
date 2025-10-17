(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Arrays;

Const Count = 20;

Var Numbers: Array[1..Count] of Integer;
    Strings: Array[1..Count] of String;
    i, n: Integer;

{ Same as Free Pascal SysUtils }
Function Int2Str(number: Integer): String;
Var s: String;
    digit: Integer;
Begin
    { Convert an Integer to a string }
    { This is a simplified version of the SysUtils.IntToStr function }
    { It does not handle all edge cases, but is sufficient for this example }
    If number = 0 Then
        Begin
            s := '0';
        End
    Else
        Begin
            s := '';
            If number < 0 Then
            Begin
                number := -number;
                s := '-';
            End;
            While number > 0 Do
            Begin
                digit := number Mod 10;
                number := number Div 10;
                s := Chr(Ord('0') + digit) + s;
            End;
        End;
    Int2Str := s;
End;

function Pad(n: Integer; w: Integer): String;
Var s: String;
Begin
    s := Int2Str(n);
    while Length(s) < w do
    Begin
        s := '0' + s;
    End;
    Pad := s;
End;

Begin
    n := 1;
    For i := 1 To Count Do
    Begin
        Numbers[i] := n;
        Strings[i] := '#' + Pad(i, 2) + ' ' + Int2Str(n);
        n := n * 2;
    End;
    WriteLn('#  | Number | String    ');
    WriteLn('---|--------|-----------');
    For i := 1 To Count Do
    Begin
        WriteLn(Pad(i, 2), ' | ', Numbers[i]:6, ' | ', Strings[i]);
    End;
End.
