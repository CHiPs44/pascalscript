(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example004TestExpr;

Const
  AAA = 123;
  BBB = -234;
  CCC = 345;//AAA; <== TODO CORE DUMP!
  BIN = %01010101;
  OCT = &7777;
  HEX = $55AA;

Var
  Test1: Integer;
  Test2: Integer;
  Test3: Boolean;
  Test4: Unsigned;

Begin
  Test1 := AAA + 3 * 45 Div BIN;
  WriteLn(Test1);
  Test2 := BBB * 3 + OCT + 3 Div 4;
  WriteLn(Test2);
  Test3 := Test1 * 12 >= Test2;
  WriteLn(Test3);
  //Test4 := CCC;//(1 * CCC + 3 * 4) + HEX; <== TODO OUT OF RANGE!
  //WriteLn(Test4);
End.
