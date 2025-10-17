(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example06ForDo;
Const 
  Limit = 99;
  PerLine = 25;
Var 
  I: Integer;
  C: Char;
  BOL: Boolean;
Begin
  WriteLn('Example 06: For-Do Loop');
  WriteLn('--------------------------------------------------------------------------------');
  WriteLn('From 0 to ', Limit, ' with leading zeroes, ', PerLine, ' per line:');
  WriteLn('--------------------------------------------------------------------------------');
  BOL := False;
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
  WriteLn('From ''A'' to ''Z'' with ordinal values:');
  WriteLn('--------------------------------------------------------------------------------');
  I := 0;
  BOL := False;
  For C := 'A' To 'Z' Do
    Begin
      Write('''', C, '''=#', Ord(C));
      I := I + 1;
      If I Mod 10 = 0 Then
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
  if Not BOL Then
    WriteLn;
  WriteLn('OK!');
  WriteLn('--------------------------------------------------------------------------------');
End.
