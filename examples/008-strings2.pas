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

Var
    C: Char;
    s1, s2, s3, s4: String;
    I: Integer;

Procedure WriteOkOrFailed(Condition: Boolean);
Begin
    Write(' => ');
    If Condition Then Write('OK!') Else Write('FAILED!');
    WriteLn;
End;

Begin
    WriteLn('*** PascalScript version ', PS_VERSION, ' ', PS_BITNESS, ' bits ***');
    WriteLn('------------------------------');
    WriteLn('String constants and functions');
    WriteLn('------------------------------');
    Write(HelloWorld);
    WriteOkOrFailed(HelloWorld = 'Hello, World!');
    s1 := 'Hello';
    Write('s1 = ', s1);
    WriteOkOrFailed(s1 = 'Hello');
    s2 := 'World';
    Write('s2 = ', s2);
    WriteOkOrFailed(s2 = 'World');
    s3 := s1 + ', ' + s2 + '!';
    Write('s3 = s1 + s2 + ! = ', s3);
    WriteOkOrFailed(s3 = 'Hello, World!');
    s4 := '';
    For I := 1 to 10 do
        s4 := s4 + '-_';
    Write('s4 = ', s4);
    WriteOkOrFailed(s4 <> '-_-_-_-_-_-_-_-_-_-');
    Write('Length(''', s1, ''') = ', Length(s1));
    WriteOkOrFailed(Length(s1) = 5);
    Write('Length(''', s2, ''') = ', Length(s2));
    WriteOkOrFailed(Length(s2) = 5);
    Write('LowerCase(''', s2, ''') = ', LowerCase(s2));
    WriteOkOrFailed(LowerCase(s2) = 'world');
    Write('UpperCase(''', s1, ''') = ', UpperCase(s1));
    WriteOkOrFailed(UpperCase(s1) = 'HELLO');
    C := LowerCase('A');
    Write('LowerCase(''A'') = ''', C,'''');
    WriteOkOrFailed(C = 'a');
    C := UpperCase('a');
    Write('UpperCase(''a'') = ''', C,'''');
    WriteOkOrFailed(C = 'A');
    C := 'A';
    S1 := C;
    Write('S1 = C = "', S1, '"');
    WriteOkOrFailed('A' = S1);
    Write(' ');
    WriteOkOrFailed(C  = S1);
    Write(' ');
    WriteOkOrFailed(S1 = C);
    Write(' ');
    WriteOkOrFailed(S1 = 'A');
    // *FUTURE*
    // WriteLn('Pos(''lo'', s1) = ', Pos('lo', s1));
    // WriteLn('Pos(''lo'', s2) = ', Pos('lo', s2));
End.
