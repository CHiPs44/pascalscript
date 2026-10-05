(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program TestWriteStr;

Var
  S: String;

Begin
    S := 'Hello, World!';
    WriteStr(S, '1. This is a test', Pi:10:7, ' ', S);
    WriteLn('>>>', S, '<<<');
    WriteStr(S, '2. This is a test', Pi:10:7, ' ', S);
    WriteLn('>>>', S, '<<<');
End.
