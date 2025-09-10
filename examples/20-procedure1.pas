(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
{       0        1         2         3  }
{       1234567890123456789012345678901 }
Program Example20Procedure1;

Var
    Variable1: Integer;
    I: Integer;
    Variable2: Integer;

Procedure Procedure1(Parameter1, Parameter2: Integer);
Var
    Variable1: Integer;
Begin
    Variable1 := Parameter1 * 2;
    WriteLn('    a. This is Procedure1, Parameter1=', Parameter1,'Variable1=', Variable1);
End;

// Procedure Procedure2(Variable2: Integer);
// Begin
//     WriteLn('    1. This is Procedure2, Variable2=', Variable2);
//     Variable2 := 12;
//     WriteLn('    2. This is Procedure2, Variable2=', Variable2);
// End;

Begin
    Variable1 := 1;
    For I := 1 to 3 Do
    Begin
        WriteLn(I, '. This is Program, Variable1=', Variable1, ', Parameter=', I*10);
        // Procedure1(I*10, 42);
    End;
    // WriteLn('------------------------------');
    // Variable2 := 0;
    // WriteLn('1. This is Program, Variable2=', Variable2);
    // Procedure2(Variable2);
    // WriteLn('2. This is Program, Variable2=', Variable2);
End.
