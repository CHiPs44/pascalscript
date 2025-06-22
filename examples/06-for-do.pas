(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example06ForDo;
Const
    Limit = 99;
Var
    I: Integer;
    C: Char;
    BOL: Boolean;
Begin
    // BOL := False;
    // For I := 0 To Limit Do
    // Begin
    //     if I < 10 Then
    //         Write('0');
    //     Write(I);
    //     If I > 0 And (I + 1) Mod 10 = 0 Then
    //     Begin
    //         WriteLn;
    //         BOL := True;
    //     End
    //     Else
    //     Begin
    //         Write(' ');
    //         BOL := False;
    //     End;
    // End;
    // If Not BOL Then
    //     WriteLn;    
    // WriteLn('OK!');
    I := 0;
    For C := 'A' To 'Z' Do
    Begin
        Write('''', C, '''=#', Ord(C));
        // Write('''', C, '''');
        I := I + 1;
        If I Mod 13 = 0 Then
            WriteLn
        Else
            Write(' ');
    End;
    WriteLn('OK!');
End.
