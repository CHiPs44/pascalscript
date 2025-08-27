(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
{       1234567890123456789012345678901 }
Program Example20Procedure1;

Var
    Variable1, Parameter, I: Integer;

Procedure Procedure1;
Var
    Variable1: Integer;
Begin
    Variable1 := Parameter * 2;
    WriteLn('    a. This is Procedure1, Variable1=', Variable1);
End;

Begin
    Variable1 := 1;
    For I := 1 to 3 Do
    Begin
        Parameter := I * 10;
        WriteLn(I, '. This is Program, Variable1=', Variable1, ', Parameter=', Parameter);
        Procedure1;
    End;
End.
