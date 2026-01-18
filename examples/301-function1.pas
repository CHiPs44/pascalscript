(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)

Program Example300Function1;

Function FooBarBaz(pCount, Sides: Unsigned): Unsigned;
Var
    Test: Unsigned;
    Total: Unsigned;
Begin
    Test := 234567;
    // Write(' FooBarBaz: Count=', Count);
    Total := 0;
    While pCount > 0 Do
    Begin
        Total := Total + Random(Sides) + 1;
        pCount := pCount - 1;
    End;
    FooBarBaz := Total;
    // Result := Total;
End;

Function D6(Count: Unsigned): Unsigned;
Begin
    D6 := FooBarBaz(Count, 6);
    // D6 := 3 + Random(6) + Random(6) + Random(6);
End;

Const
    Count = 256;

Var
    Test: integer;
    I: Unsigned;
    Characteristic, Minimum, Maximum: Unsigned;
    Total2: Real;

Begin
    Test := 123456;
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('Launching 3d6 for ', Count, ' tries...');
    Randomize;
    Minimum := MaxInt;
    Maximum := 0;
    Total2 := 0;
    For I := 1 To Count Do
    Begin
        // Characteristic := D6(3);
        Characteristic := FooBarBaz(3, 6);
        Total2 := Total2 + Characteristic;
        If Characteristic < Minimum Then Minimum := Characteristic;
        If Characteristic > Maximum Then Maximum := Characteristic;
        if Characteristic < 10 Then Write(' ');
        Write(Characteristic, ' ');
        If I Mod 16 = 0 Then WriteLn(' Mean = ', Total2 / I, ' Total2 = ', Total2);
    End;
    WriteLn;
    WriteLn('Minimum = ', Minimum, ', Maximum = ', Maximum, ', Mean = ', Total2 / Count);
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('Test=', Test);
End.
