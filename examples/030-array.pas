(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Arrays;

Const
    One = 1;
    ValueCount = 10;

Type
//     TIndex1 = 1..10;
//     TIndex2 = 1..ValueCount;
//     TIndex3 = One..ValueCount;
//     TIndex4 = -4..4;
//     TIndex5 = -One..ValueCount;
    NumberArray = Array[1..ValueCount] Of Integer;
    // StringArray = Array[1..ValueCount] Of String;

// { Same as Free Pascal SysUtils }
// Function IntToStr(N: Integer): String;
// Var S: String;
//     Digit: Integer;
// Begin
//     { Convert an Integer to a string }
//     { This is a simplified version of the SysUtils.IntToStr function }
//     { It does not handle all edge cases, but is sufficient for this example }
//     If N = 0 Then
//         S := '0'
//     Else
//     Begin
//         S := '';
//         If N < 0 Then
//         Begin
//             N := -N;
//             S := '-';
//         End;
//         While N > 0 Do
//         Begin
//             Digit := N Mod 10;
//             S := Chr(Ord('0') + Digit) + S;
//             N := N Div 10;
//         End;
//     End;
//     IntToStr := S;
// End;

// function Pad(n: Integer, w: Integer): String;
// Var S: String;
// Begin
//     S := IntToStr(n);
//     while Length(S) < w do
//     Begin
//         S := '0' + S;
//     End;
//     Pad := S;
// End;

Var
    Numbers: NumberArray;
    // Strings: StringArray;
    // i, n: Integer;
    // s: String;

Begin
    Numbers[1] = 1;
    // n := 1;
    // // For i := 1 To ValueCount Do
    // // Begin
    // //     Numbers[i] := n;
    // //     Strings[i] := '#' + Pad(i, 2) + ' ' + IntToStr(n);
    // //     n := n * 2;
    // // End;
    // WriteLn('#  | Number | String    ');
    // WriteLn('---|--------|-----------');
    // For i := 1 To ValueCount Do
    // Begin
    //     // WriteLn(Pad(i, 2), ' | ', Numbers[i]:6, ' | ', Strings[i]);
    //     s := IntToStr(i);
    //     WriteLn(i:2, ' | ', i:6, ' | ', s);
    // End;
End.
