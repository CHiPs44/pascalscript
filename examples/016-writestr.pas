(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program TestWriteStr;

Var
  S: String;

Begin
    // WriteStr(S, 'TEST1TEST1TEST1TEST1TEST1', '|TEST2TEST2TEST2TEST2TEST2', '|TEST3TEST3TEST3TEST3TEST3');
    // WriteLn('>>>', S, '<<<');
    WriteLn('--------------------------------------------------------------------------------');
    S := '0. ' + 'Hello, World!' + ' 123';
    WriteLn('      123456789012345678901234567890123456789012345678901234567890');
    WriteLn('xxx>>>', S, '<<<');
    WriteLn('--------------------------------------------------------------------------------');
    WriteStr(S, '1. This is test 1 ', Pi / 4.0     :10:7, ' 12345678901234567890');
    WriteLn('      123456789012345678901234567890123456789012345678901234567890');
    WriteLn('yyy>>>', S, '<<<');
    WriteLn('--------------------------------------------------------------------------------');
    WriteStr(S, '2. This is test 2 ', Sqrt(2.0)/2.0:10:7, ' 1234567890');
    WriteLn('      123456789012345678901234567890123456789012345678901234567890');
    WriteLn('zzz>>>', S, '<<<');
    WriteLn('--------------------------------------------------------------------------------');
End.
