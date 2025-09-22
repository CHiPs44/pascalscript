(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example22Procedure2;

Var
    Variable1, I: Integer;
    Variable2: Integer;

Procedure WriteLine(Width: Unsigned);
Var
    I: Unsigned;
Begin
    For I := 1 to Width Do
        Write('-');
    WriteLn;
End;

Procedure Sum(A, B: Integer, Var C: Unsigned);
Begin
    C := A + B;
End;

Begin
    WriteLine(80);
    Variable1 := 0;
    WriteLn('Before Sum, Variable1=', Variable1);
    Sum(10, 32, Variable1);
    WriteLn('After Sum, Variable1=', Variable1);
    WriteLn('Sum procedure');
    WriteLine(80);
End.
