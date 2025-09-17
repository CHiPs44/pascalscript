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
  T1, T2: Unsigned;
Begin
  WriteLn('Starting big loops...');
  WriteLn('----------------------------------------------------------------------');
  T1 := Ticks();
  WriteLn('T1=', T1);
  For I := 1 To MaxLoops Do
  Begin
    // Just a big loop
  End;
  T2 := Ticks();
  WriteLn('T2=', T2);
  WriteLn('*FOR* loop took ', T2 - T1, ' ms for ', I - 1, ' iterations');
  WriteLn('----------------------------------------------------------------------');
  T1 := Ticks();
  WriteLn('T1=', T1);
  I := 1;
  Repeat
    I := I + 1;
  Until I > MaxLoops;
  T2 := Ticks();
  WriteLn('T2=', T2);
  WriteLn('*REPEAT* loop took ', T2 - T1, ' ms for ', I - 1, ' iterations');
  WriteLn('----------------------------------------------------------------------');
  T1 := Ticks();
  WriteLn('T1=', T1);
  I := 1;
  While I <= MaxLoops Do
  Begin
    I := I + 1;
  End;
  T2 := Ticks();
  WriteLn('T2=', T2);
  WriteLn('*WHILE* loop took ', T2 - T1, ' ms for ', I - 1, ' iterations');
  WriteLn('----------------------------------------------------------------------');
  WriteLn('Finished big loops.');
  WriteLn('OK!');
End.
