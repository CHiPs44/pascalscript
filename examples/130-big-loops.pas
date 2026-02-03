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
  Start, Finish: Unsigned;
  T1For, T2For: Unsigned;
  T1While, T2While: Unsigned;
  T1Repeat, T2Repeat: Unsigned;
Begin
  Start := GetTickCount();
  WriteLn('Starting big loops...');

  T1For := GetTickCount();
  For I := 1 To MaxLoops Do
  Begin
  End;
  T2For := GetTickCount();

  T1Repeat := GetTickCount();
  I := 1;
  Repeat
    I := I + 1;
  Until I > MaxLoops;
  T2Repeat := GetTickCount();

  T1While := GetTickCount();
  I := 1;
  While I <= MaxLoops Do
  Begin
    I := I + 1;
  End;
  T2While := GetTickCount();

  WriteLn('PascalScript - Comparison of execution times for ', MaxLoops,' iterations:');
  WriteLn(' - for         : ', T2For - T1For, ' ms');
  WriteLn(' - while       : ', T2While - T1While, ' ms');
  WriteLn(' - repeat until: ', T2Repeat - T1Repeat, ' ms');

  WriteLn('Finished big loops.');
  Finish := GetTickCount();
  WriteLn('Overall time: ', Finish - Start, ' ms');
  WriteLn('OK!');
End.
