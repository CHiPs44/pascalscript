(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)

Program Example30Function0;

Function Dice(Count, Sides: Integer): Integer;
Var
    Total: Integer;
Begin
    Total := 0;
    While Count > 0 Do
    Begin
        Total := Total + Random(Sides) + 1;
        Count := Count - 1;
    End;
    Dice := Total;
End;

Function D6(Count: Integer): Integer;
Begin
    D6 := Dice(Count, 6);
    // Result := Dice(Count, 6);
End;

Const
    Count = 256;

Var
    Characteristic, Min, Max, I: Integer;
    Total: Real;

Begin
    WriteLn('--------------------------------------------------------------------------------');
    Randomize;
    Min := MaxInt;
    Max := 0;
    Total := 0;
    For I := 1 To Count Do
    Begin
        Characteristic := D6(3);
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
