(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program ForDo;
Const
    Limit = 99;
    PerLine = 25;
Var
    I: Integer;
    C: Char;
    BOL: Boolean;
Begin
    WriteLn('Example 024: For-Do Loop');
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('1. From 0 to ', Limit, ' with leading zeroes, ', PerLine, ' per line:');
    WriteLn('--------------------------------------------------------------------------------');
    BOL := True;
    For I := 0 To Limit Do
    Begin
        If I < 10 Then
            Write('0');
        Write(I);
        If I > 0 And (I + 1) Mod PerLine = 0 Then
        Begin
            WriteLn;
            BOL := True;
        End
        Else
        Begin
            Write(' ');
            BOL := False;
        End;
    End;
    If Not BOL Then
        WriteLn;
    WriteLn('OK!');
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('2. From ''Z'' to ''A'' with ordinal values:');
    WriteLn('--------------------------------------------------------------------------------');
    I := 0;
    BOL := True;
    For C := 'Z' Downto 'A' Do
    Begin
        Write('''', C, '''=#', Ord(C));
        I := I + 1;
        BOL := I Mod 10 = 0;
        If BOL Then
            WriteLn
        Else
            Write(' ');
    End;
    If Not BOL Then
        WriteLn;
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('OK!');
End.
