(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Second;
Const
    FOO = 10;
    BAR = 20;
Var
    A, B, C, D: Integer;
    R: Real;
Begin
    A := FOO;
    B := BAR;
    // R := A * B + 1.23456789;
    // WriteLn('R=', R);
    C := A + B;
    // WriteLn('C=', C);
    D := (A * B) Div C;
    // WriteLn('D=', D);
    // // KO
    // R := (A * B * 1.0) / (C * 1.0);
    // // R := (1.0 * A * B) / (1.0 * C);
    R := A * B * 1.0;
    WriteLn(
        'FOO=', FOO, 
        ', BAR=', BAR, 
        ', A=FOO=', A, 
        ', B=BAR=', B, 
        ', C=A+B=', C, 
        ', D=A*B/C=', D, 
        ', R=A*B+1=', R//:0:2
    );
    // // FOO := 12;
End.
