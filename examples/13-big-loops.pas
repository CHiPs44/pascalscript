(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example13BigLoops;
Var
  I: Integer;
Begin
  WriteLn('Starting big loops...');
  For I := 1 to 10000000 do
  Begin
  End;
  WriteLn('Finished big loops.');
  WriteLn('OK!');
End.
