(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program FunctionInWriteLn;

Function F(A, B: Integer): Integer;
Begin
  F := A + B;
End;

Var
  I: Integer;

Begin
  I := 1 + F(12, 34); // <== OK
//   I := F(F(12, 34), 56) + 1;
//   I := I + 1;
//   WriteLn(I);
//WriteLn(F(12, 34));
//WriteLn(F(12, 34), ' trailing');
//WriteLn('prefix: ', F(12, 34));
//WriteLn('prefix: ', F(12, 34), ' trailing');
//WriteLn(F(1, 2), ' + ', F(3, 4));
//Write(F(10, 20));
//WriteLn(' done');
End.
