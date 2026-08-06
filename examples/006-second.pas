(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Second;

Const
    FOO = 11;
    BAR = 22;
    BAZ = 65.4321;

Var
    A, B, C, D: Integer;
    R0, R1, R2, R3, R4: Real;

Begin
    WriteLn('FOO=', FOO, ', BAR=', BAR, ', BAZ=', BAZ);
    A := FOO;
    B := BAR;
    C := A + B;
    D := (A * B) Div C;
    WriteLn('A=', A, ', B=', B, ', C=', C, ', D=', D);
    R0 := (A * B) / C;
    R1 := FOO * BAR * BAZ;
    R2 := FOO + BAR + BAZ;
    R3 := FOO * BAR + BAZ;
    R4 := FOO + BAR * BAZ;
    WriteLn('R0=', R0, ', R1=', R1, ', R2=', R2, ', R3=', R3, ', R4=', R4);
    // FOO := 12;
End.
