(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example30Function0;

Var
    Strength: Unsigned;
    // Global1: Unsigned;

Function Dice(Count: Unsigned, Sides: Unsigned): Unsigned;
Begin
    Result := 0;
    While Count > 0 Do
    Begin
        Result := Result + 1 + Random(Sides);
        Count := Count - 1;
    End;
End;

Function d6(Count: Unsigned): Unsigned;
Begin
    // d100 := Random(100) + 1;
    Result := Dice(Count, 6);
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
    Randomize;
    // Strength := Random(100) + 1;
    // WriteLn('Strength=', Strength);
    Strength := d6(3);
    WriteLn('Strength=', Strength);
    // Global1 := 123456789;
    // WriteLn('Global1=', Global1);
    // Global1 := SumU(123456789, 987654321);
    // WriteLn('Global1=', Global1);
    WriteLn('--------------------------------------------------------------------------------');
End.
