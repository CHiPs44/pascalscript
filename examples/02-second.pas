(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Second;
Const 
    FOO = 100;
Var
    a, b, c, d: Integer;
    r: Real;
Begin
    a := FOO;
    // WriteLn('a=', a);
    b := 20;
    // WriteLn('b=', b);
    c := a + b;
    // WriteLn('c=', c);
    d := (a * b);// Div c;
    // WriteLn('d=', d);
    // KO
    // r := (a * b * 1.0) / (c * 1.0);
    // r := (1.0 * a * b) / (1.0 * c);
    r := a * b / 10.0;// + 1.0;
    WriteLn(
        'FOO=', FOO, 
        ',a=FOO=', a, 
        ', b=20=', b, 
        ', c=a+b=', c, 
        ', d=a*b/c=', d, 
        ', r=a*b+1=', r//:0:2
    );
    // FOO := 12;
End.
