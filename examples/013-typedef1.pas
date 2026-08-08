(*
        This file is part of the PascalScript Pascal interpreter.
        SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
        SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program TypeDef1;

Type
    Subrange1To10 = 1..10;
    Letter = 'A'..'Z';

Var
    N: vSubrange1To10;
    L: Letter;

Begin
    N := 5;
    WriteLn(N);
    L := 'X';
    WriteLn(L);
    // N := 42; // <= ERROR 177 Out of range
    // WriteLn(N);
End.
