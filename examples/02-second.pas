(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Second;
Const 
    Foo = 100;
Var
    a, b, c, d: Integer;
    r: Real;
Begin
    a := Foo;
    WriteLn('a=', a);
    b := 20;
    WriteLn('b=', b);
    c := 234;
    c := a + b + c;
    WriteLn('c=', c);
    d := (a * b) Div c;
    WriteLn('d=', d);
    r := (a * b * 1.0) / (c * 1.0);
    WriteLn('r=', r);
    // Foo := 12;
End.
