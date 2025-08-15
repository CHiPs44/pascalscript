(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
{       1234567890123456789012345678901 }
Program Example20Procedure1;

Var
    Variable1: Integer;

Procedure Procedure1;
Var
    Variable1: Integer;
Begin
    Variable1 := 42;
    WriteLn('    a. This is Procedure1, Variable1=', Variable1);
End;

Begin
    Variable1 := 1;
    WriteLn('1. This is Program, Variable1=', Variable1);
    Procedure1;
    WriteLn('2. This is Program, Variable1=', Variable1);
    Procedure1;
    WriteLn('3. This is Program, Variable1=', Variable1);
End.
