(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)

Program Example302Function2;

Var
    Test: integer;

Procedure Local1;
Var
    Test: Char;
Begin
    WriteLn('   Local1: Test=', Test);
    Test := 'A';
    WriteLn('   Local1: Test=', Test);
End;

Procedure Local2;
Var
    Test: Unsigned;
Begin
    WriteLn('   Local2: Test=', Test);
    Test := 222222;
    WriteLn('   Local2: Test=', Test);
    Local1;
    WriteLn('   Local2: Test=', Test);
End;

Begin
    WriteLn('--------------------------------------------------------------------------------');
    Test := 999999;
    WriteLn('   Global: Test=', Test);
    Local1;
    WriteLn('   Global: Test=', Test);
    Local2;
    WriteLn('   Global: Test=', Test);
    WriteLn('--------------------------------------------------------------------------------');
End.
