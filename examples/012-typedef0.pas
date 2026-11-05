(*
        This file is part of the PascalScript Pascal interpreter.
        SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
        SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program TypeDef0;

Type
  MyInteger1 = Integer;
  MyInteger2 = MyInteger1;

Var
  I : MyInteger1;
  J : MyInteger2;
  K : Integer;

Begin
  I := 42;
  Write(I, ' I <= 42: ');
  If I <= 42 Then WriteLn('OK') Else WriteLn('FAILED!');
  J := I + 1;
  Write(J, ' J <= 43: ');
  If J <= 43 Then WriteLn('OK') Else WriteLn('FAILED!');
  K := J + 1;
  Write(K, ' K <= 44: ');
  If K <= 44 Then WriteLn('OK') Else WriteLn('FAILED!');
End.
