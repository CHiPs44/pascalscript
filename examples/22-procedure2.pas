(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example22Procedure2;

Var
    Start, Stop: Unsigned;
    Global1: Unsigned;

Procedure WriteLine(Ch: Char, Width: Unsigned);
Var
    I: Unsigned;
Begin
    For I := 1 to Width Do
        Write(Ch);
    WriteLn;
End;

Procedure Sum(A, B: Unsigned, Var C: Unsigned);
Begin
    // C := A + B;
End;

Begin
    Start := GetTickCount();
    WriteLn('Started at ', Start, ' ms');
    WriteLine('-', 80);
    Global1 := 0;
    WriteLn('Before Sum, Global1=', Global1);
    // Sum(10, 32, Global1);
    WriteLn('After  Sum, Global1=', Global1);
    WriteLine('-', 80);
    Stop := GetTickCount();
    WriteLn('Stopped at ', Stop, ' ms');
    WriteLn('ElapsedTime=', Stop - Start, ' ms');
End.
