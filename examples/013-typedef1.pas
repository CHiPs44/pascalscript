(*
        This file is part of the PascalScript Pascal interpreter.
        SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
        SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program TypeDef1;

Type
    Subrange = 1..10;

Var
    N: Subrange;

Begin
    N := 1;
    WriteLn(N);
End.
