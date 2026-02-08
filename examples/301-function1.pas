(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)

Program Example301Function1;

Const
    Count = 16;

Function RollDices(Count, Sides: Unsigned): Unsigned;
Var
    Total: Unsigned;
Begin
    Total := 0;
    While Count > 0 Do
    Begin
        Total := Total + Random(Sides) + 1;
        Count := Count - 1;
    End;
    // RollDices := Total
    Result := Total
End;

Function D6(Count: Unsigned): Unsigned;
Var
    Total: Unsigned;
Begin
    Total := RollDices(Count, 6);
    // Total := 3 + Random(6) + Random(6) + Random(6);
    D6 := Total
End;

Var
    I: Unsigned;
    Characteristic, Minimum, Maximum, Count2: Unsigned;
    Total2: Real;

Begin
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('Launching 3d6 for ', Count, ' tries (until minimum=3 and maximum=18)...');
    WriteLn('Theorical mean = ', 10.5);
    Randomize;
    Minimum := 18;
    Maximum := 3;
    Total2 := 0;
    Count2 := 0;
    Repeat
        For I := 1 To Count Do
        Begin
            Characteristic := D6(3);
            // Characteristic := RollDices(3, 6);
            Total2 := Total2 + Characteristic;
            If Characteristic < Minimum Then Minimum := Characteristic;
            If Characteristic > Maximum Then Maximum := Characteristic;
            // if Characteristic < 10 Then Write(' ');
            Write(Characteristic:2, ' ');
            Count2 := Count2 + 1;
            If I Mod 16 = 0 Then WriteLn(' Mean = ', (Total2 / Count2):8:4, ' Total2 = ', Total2:10:2);
        End;
    Until (Minimum = 3 And Maximum = 18) Or (Count2 >= 1000);
    WriteLn;
    WriteLn('Minimum = ', Minimum, ', Maximum = ', Maximum, ', Mean = ', Total2 / Count2:8:4, ' Count = ', Count2);
    WriteLn('--------------------------------------------------------------------------------');
End.
