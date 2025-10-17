(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example22Procedure2;

Var
    // Start, Stop: Unsigned;
    Global1, Global2: Unsigned;

// { "Real" usable proceduree }
// Procedure WriteLine(Ch: Char, Width: Unsigned);
// Var
//     I: Unsigned;
// Begin
//     For I := 1 to Width Do
//         Write(Ch);
//     WriteLn;
// End;

{ Not so usable procedure, just to show parameters passing }
Procedure Ops(A, B: Unsigned, Var C, D: Unsigned);
Begin
    WriteLn('    In Ops #1, Global1=', Global1, ' Global2=', Global2);
    WriteLn('        A=', A, ' B=', B, ' C=', C, ' D=', D);
    C := A + B;
    D := A * B;
    WriteLn('        A=', A, ' B=', B, ' C=', C, ' D=', D);
    WriteLn('    In Ops #2, Global1=', Global1, ' Global2=', Global2);
End;

Begin
    // Start := GetTickCount();
    // WriteLine('-', 80);
    Global1 := 123456789;
    Global2 := 987654321;
    WriteLn('Before Ops, Global1=', Global1, ' Global2=', Global2);
    Ops(10, 32, Global1, Global2);
    // WriteLn('After  Ops, Global1=', Global1, ' Global2=', Global2);
    // WriteLine('-', 80);
    // Stop := GetTickCount();
    // WriteLn('Started at ', Start, ' ms');
    // WriteLn('Stopped at ', Stop, ' ms');
    // WriteLn('Elapsed time ', Stop - Start, ' ms');
End.
