(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)

Program Example30Function0;

Function FooBarBaz(pCount, Sides: Integer): Integer;
Var
    Total: Integer;
Begin
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

Function D6(Count: Integer): Integer;
Var
    Temp: Integer;
Begin
    // Write(' D6: Count=', Count);
    // D6 := FooBarBaz(Count, 6);
    // Result := FooBarBaz(Count, 6);
    Temp := FooBarBaz(Count, 6);
    D6 := Temp;
End;

Const
    Count = 16;

Var
    I: Integer;
    Characteristic, Min, Max: Integer;
    Total: Real;

Begin
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('Launching 3d6 for ', Count, 'tries...');
    Randomize;
    Min := MaxInt;
    Max := 0;
    Total := 0;
    For I := 1 To Count Do
    Begin
        // Characteristic := D6(3);
        Characteristic := FooBarBaz(3, 6);
        Total := Total + Characteristic;
        If Characteristic < Min Then Min := Characteristic;
        If Characteristic > Max Then Max := Characteristic;
        if Characteristic < 10 Then Write(' ');
        Write(Characteristic, ' ');
        If I Mod 16 = 0 Then WriteLn(' Mean = ', Total / I);
    End;
    WriteLn;
    WriteLn('Min = ', Min, ', Max = ', Max, ', Mean = ', Total / Count);
    WriteLn('--------------------------------------------------------------------------------');
End.
