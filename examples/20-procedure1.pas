(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example20Procedure1;

Procedure Procedure1;
Var
    Variable1: Integer;
Begin
    Variable1 := 42;
    WriteLn('This is Procedure1, Variable1=', Variable1);
End;

Begin
    Procedure1;
End.
