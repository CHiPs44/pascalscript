(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Second;
Const
    FOO = 1;
    BAR = 2;
    BAZ = 3.0;
Var
    A, B, C, D: Integer;
    R: Real;
Begin
    // A := FOO;
    // B := BAR;
    // // R := A * B + 1.23456789;
    // // WriteLn('R=', R);
    // C := 0;//A + B;
    // // WriteLn('C=', C);
    // D := 0;//(A * B) Div C;
    // // WriteLn('D=', D);
    // // // KO
    // // R := (A * B * 1.0) / (C * 1.0);
    // // // R := (1.0 * A * B) / (1.0 * C);
    // R := A * B / 2.0;// / BAZ;
    R := FOO * BAR * BAZ;
    WriteLn(
        'FOO=', FOO, 
        ', BAR=', BAR, 
        ', BAZ=', BAZ, 
        ', A=', A, 
        ', B=', B, 
        ', C=', C, 
        ', D=', D, 
        ', R=', R//:0:2
    );
    // FOO := 12;
End.
