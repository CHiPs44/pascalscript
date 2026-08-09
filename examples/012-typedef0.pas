(*
        This file is part of the PascalScript Pascal interpreter.
        SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
        SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program TypeDef0;

Type
    MyInteger = Integer;

Var
    I : MyInteger;

Begin
    I := 42;
    WriteLn(I);
End.
