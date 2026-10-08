(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program TestWriteStr;

Var
  S: String;

Begin
    WriteLn('--------------------------------------------------------------------------------');
    S := '0. Hello, World!';
    WriteLn('>>>', S, '<<<');
    WriteLn('--------------------------------------------------------------------------------');
    WriteStr(S, '1. This is test 1 ', Pi / 4.0     :10:7);
    WriteLn('>>>', S, '<<<');
    WriteLn('--------------------------------------------------------------------------------');
    WriteStr(S, '2. This is test 2 ', Sqrt(2.0)/2.0:10:7);
    WriteLn('>>>', S, '<<<');
    WriteLn('--------------------------------------------------------------------------------');
End.
