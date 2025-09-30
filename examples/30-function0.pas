(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example30Function0;

Function Dice(Count, Sides: Unsigned): Unsigned;
Begin
    Result := 0;
    While Count > 0 Do
    Begin
        Result := Result + Random(Sides) + 1;
        Count := Count - 1;
    End;
End;

Function D6(Count: Unsigned): Unsigned;
Begin
    // D6 := Dice(Count, 6);
    Result := Dice(Count, 6);
End;

Const
    Count = 1000;

Var
    Characteristic, Min, Max, I: Unsigned;

Begin
    WriteLn('--------------------------------------------------------------------------------');
    Randomize;
    Min := MaxUInt;
    Max := 0;
    For I := 1 To Count Do
    Begin
        Characteristic := D6(3);
        if Characteristic < Min then Min := Characteristic;
        if Characteristic > Max then Max := Characteristic;
        Write(Characteristic, ' ');
    End;
    WriteLn;
    WriteLn('Min = ', Min);
    WriteLn('Max = ', Max);
    WriteLn('--------------------------------------------------------------------------------');
End.
