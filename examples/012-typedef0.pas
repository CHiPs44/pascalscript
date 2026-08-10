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

Begin
    I := 42;
    WriteLn(I);
    J := 43;
    WriteLn(J);
End.
