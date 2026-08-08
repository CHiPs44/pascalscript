(*
        This file is part of the PascalScript Pascal interpreter.
        SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
        SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program TypeDef1;

// Const
//     ONE = 1;
//     TEN = 10;

Type
    MyInteger = Integer;
    // Subrange1 = 1..10;
    // Subrange2 = ONE..TEN;
    // Letter = 'A'..'Z';

Var
    I : MyInteger;
    // N1: Subrange1;
    // N2: Subrange2;
    // L: Letter;

Begin
    // I := 42;
    // WriteLn(I);
    // N1 := 5;
    // WriteLn('N1=', N1);
    // N2 := 7;
    // WriteLn('N2=', N2);
    // L := 'X';
    // WriteLn('L=', L);
    // // N1 := 42; // <= ERROR 177 Out of range
    // // WriteLn(N1);
End.
