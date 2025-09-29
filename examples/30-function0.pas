(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example30Function0;

Var
    Strength: Integer;
    // Global1: Integer;

Function d100: Integer;
Var
    foo: Integer;
Begin
    // d100 := Random(100) + 1;
    // Result := Random(100) + 1;
    foo := 1 + Random(100);
    Result := foo;
    // Result := 42; // chosen by fair dice roll.
End;

// Function SumU(A: Integer, B: Integer): Integer;
// Var
//     C: Integer;
// Begin
//     WriteLn('In SumU A=', A, ' B=', B);
//     C := A + B;
//     WriteLn('In SumU C=', C);
//     // SumU := C;
// End;

Begin
    WriteLn('--------------------------------------------------------------------------------');
    Randomize;
    Strength := Random(100) + 1;
    WriteLn('Strength=', Strength);
    Strength := d100;
    WriteLn('Strength=', Strength);
    // Global1 := 123456789;
    // WriteLn('Global1=', Global1);
    // Global1 := SumU(123456789, 987654321);
    // WriteLn('Global1=', Global1);
    WriteLn('--------------------------------------------------------------------------------');
End.
