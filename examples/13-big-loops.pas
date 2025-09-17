(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example13BigLoops;
Var
  I: Integer;
  T1, T2: Unsigned;
Begin
  WriteLn('Starting big loops...');
  T1 := Ticks();
  WriteLn('T1=', T1);
  For I := 1 To 1000000 Do
  Begin
    // Just a big loop
  End;
  T2 := Ticks();
  WriteLn('T2=', T2);
  WriteLn('Loop 1 took ', T2 - T1, ' ms for ', I - 1, ' iterations');
  WriteLn('Finished big loops.');
  WriteLn('OK!');
End.
