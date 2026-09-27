(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program ExampleProcedure0;

Var
    R: Real;
    G: Integer;

Procedure Procedure0;
Var
    R: Integer; // Shadows global variable with another type
Begin
    // Use global variable value
    R := G * 42;
    // Change global variable value
    G := 234;
    WriteLn('    This is Procedure0         R=', R:11, ' G=', G);
End;

Begin
    R := Pi;
    G := 123;
    WriteLn('This is the main program R=', R:10:9, ' G=', G);
    Procedure0;
    WriteLn('This is the main program R=', R:10:9, ' G=', G);
End.
