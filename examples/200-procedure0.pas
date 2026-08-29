(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program ExampleProcedure0;

Var
    N: Real;
    G: Integer;

Procedure Procedure0;
Var
    N: Integer;
Begin
    // N := G;
    N := 42;
    WriteLn('    This is Procedure0         N=', N);
    WriteLn('                               G=', G);
End;

Begin
    N := Pi;
    G := 123;
    WriteLn('This is the main program       N=', N:10:9, ' G=', G);
    Procedure0;
    WriteLn('This is the main program again N=', N:10:9, ' G=', G);
End.
