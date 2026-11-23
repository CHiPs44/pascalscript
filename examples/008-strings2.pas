(*
        This file is part of the PascalScript Pascal interpreter.
        SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
        SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Strings2;

Uses
    FreePascalCompatibility32;

Const
    HelloWorld = 'Hello, World!';

// Var
//     C: Char;
//     s1, s2, s3, s4: String;
//     I: Integer;

Begin
    // WriteLn('*** PascalScript version ', PS_VERSION, ' ', PS_BITNESS, ' bits ***');
    // WriteLn('------------------------------');
    // WriteLn('String constants and functions');
    // WriteLn('------------------------------');
    WriteLn(HelloWorld);
    If HelloWorld = 'Hello, World!' Then WriteLn('OK!') Else WriteLn('FAILED!');
    // s1 := 'Hello';
    // WriteLn('s1 = ', s1);
    // If s1 <> 'Hello' Then WriteLn('FAILED!');
    // s2 := 'World';
    // WriteLn('s2 = ', s2);
    // If s2 <> 'World' Then WriteLn('FAILED!');
    // s3 := s1 + ', ' + s2 + '!';
    // WriteLn('s3 = s1, s2! = ', s3);
    // If s3 <> 'Hello, World!' Then WriteLn('FAILED!');
    // s4 := '';
    // For I := 1 to 10 do
    //     s4 := s4 + '-_';
    // WriteLn('s4 = ', s4);
    // If s4 <> '-_-_-_-_-_-_-_-_-_-' Then WriteLn('FAILED!');
    // WriteLn('Length(''', s1, ''') = ', Length(s1));
    // WriteLn('Length(''', s2, ''') = ', Length(s2));
    // WriteLn('LowerCase(''', s2, ''') = ', LowerCase(s2));
    // WriteLn('UpperCase(''', s1, ''') = ', UpperCase(s1));
    // C := LowerCase('A');
    // WriteLn('LowerCase(''A'') = ', C);
    // C := UpperCase('a');
    // WriteLn('UpperCase(''a'') = ', C);
    // C := 'A';
    // S1 := C;
    // Write('S1 = C = "', S1, '" => ');
    // If 'A' = S1 Then Write('OK!') Else Write('FAILED!');
    // Write(' ');
    // If C   = S1 Then Write('OK!') Else Write('FAILED!');
    // Write(' ');
    // If S1 = C   Then Write('OK!') Else Write('FAILED!');
    // Write(' ');
    // If S1 = 'A' Then Write('OK!') Else Write('FAILED!');
    // WriteLn;
    // // *FUTURE*
    // // WriteLn('Pos(''lo'', s1) = ', Pos('lo', s1));
    // // WriteLn('Pos(''lo'', s2) = ', Pos('lo', s2));
End.
