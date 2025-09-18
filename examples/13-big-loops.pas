(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example13BigLoops;
Const
  MaxLoops = 100000;
Var
  I: Integer;
  T0, T1, T2, T3: Unsigned;
Begin
  T0 := GetTickCount();
  WriteLn('Starting big loops...');
  WriteLn('----------------------------------------------------------------------');
  T1 := GetTickCount();
  WriteLn('T1=', T1);
  For I := 1 To MaxLoops Do
  Begin
    // Just a big loop
  End;
  T2 := GetTickCount();
  WriteLn('T2=', T2);
  WriteLn('*FOR* loop took ', T2 - T1, ' ms for ', MaxLoops, ' iterations');
  WriteLn('----------------------------------------------------------------------');
  T1 := GetTickCount();
  WriteLn('T1=', T1);
  I := 1;
  Repeat
    I := I + 1;
  Until I > MaxLoops;
  T2 := GetTickCount();
  WriteLn('T2=', T2);
  WriteLn('*REPEAT* loop took ', T2 - T1, ' ms for ', MaxLoops, ' iterations');
  WriteLn('----------------------------------------------------------------------');
  T1 := GetTickCount();
  WriteLn('T1=', T1);
  I := 1;
  While I <= MaxLoops Do
  Begin
    I := I + 1;
  End;
  T2 := GetTickCount();
  WriteLn('T2=', T2);
  WriteLn('*WHILE* loop took ', T2 - T1, ' ms for ', MaxLoops, ' iterations');
  WriteLn('----------------------------------------------------------------------');
  WriteLn('Finished big loops.');
  T3 := GetTickCount();
  WriteLn('Overall time: ', T3 - T0, ' ms');
  WriteLn('OK!');
End.
