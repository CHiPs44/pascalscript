(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example30Function0;

// Var
    // Strength: Unsigned;
    // Global1: Unsigned;

Function d100: Unsigned;
Begin
    // d100 := Random(100) + 1;
    Result := Random(100) + 1;
End;

// Function SumU(A: Unsigned, B: Unsigned): Unsigned;
// Var
//     C: Unsigned;
// Begin
//     WriteLn('In SumU A=', A, ' B=', B);
//     C := A + B;
//     WriteLn('In SumU C=', C);
//     // SumU := C;
// End;

Begin
    WriteLn('--------------------------------------------------------------------------------');
    // Strength := d100;
    // WriteLn('Strength=', Strength);
    // Global1 := 123456789;
    // WriteLn('Global1=', Global1);
    // Global1 := SumU(123456789, 987654321);
    // WriteLn('Global1=', Global1);
    WriteLn('--------------------------------------------------------------------------------');
End.
