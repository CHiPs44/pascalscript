(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program First;
Const
    THOUSAND = 1000;
Var
    a, b, c, d: Integer;
Begin
    a := +1;
    b := -2;
    c := (a - b) * (THOUSAND + THOUSAND) Div THOUSAND;
    d := 8 * a + b;
    WriteLn('a=', a);
    WriteLn('b=', b);
    WriteLn('c=', c);
    WriteLn('d=', d);
    WriteLn('c=d? ', c = d);
    WriteLn('c<>d? ', c <> d);
End.
