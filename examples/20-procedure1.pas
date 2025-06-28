(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example20Procedure1;

// Var
//     Variable1: Integer;

Procedure Procedure1;
Var
    Variable1: Integer;
Begin
    Variable1 := 42;
    // Write('This is Procedure1, Variable1=', Variable1, Chr(10));
End;

Begin
    // Variable1 := 1;
    // WriteLn('1. This is Program, Variable1=', Variable1);
    WriteLn('1. This is Program');
    Procedure1;
    // WriteLn('2. This is Program, Variable1=', Variable1);
    WriteLn('2. This is Program');
End.
