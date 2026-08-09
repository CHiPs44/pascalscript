(*
        This file is part of the PascalScript Pascal interpreter.
        SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
        SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program TypeDef1;

Const
    ONE = 1;
    TEN = 10;

Type
    Subrange1 = 1..5;
    // Subrange1 = 42..99; // <= Compiler error: 150 Symbol already exists
    Subrange2 = ONE..TEN;
    Letter = 'A'..'Z';
    Color = (Red, Green, Blue);
    // AnsiColor = (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White);

Var
    N1: Subrange1;
    N2: Subrange2;
    L: Letter;
    C: Color;

Begin
    N1 := 5;
    WriteLn('N1=', N1);
    // N1 := 42; // <= ERROR 177 Out of range
    // WriteLn(N1);
    N2 := 7;
    WriteLn('N2=', N2);
    // N1 := N2; // <= ERROR 177 Out of range
    // WriteLn('N1=', N1);
    L := Chr(Ord('X') + 1);
    WriteLn('L=''', L, '''');
    L := Succ(L);
    WriteLn('L=''', L, '''');
    C := Green;
    WriteLn(C, ' ', Ord(C));
End.
