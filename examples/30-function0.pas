(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example30Function0;

Var
    Characteristic: Unsigned;
    I: Unsigned;

Function Dice(Count: Unsigned, Sides: Unsigned): Unsigned;
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

Begin
    WriteLn('--------------------------------------------------------------------------------');
    Randomize;
    For I := 1 To 10 Do
    Begin
        Characteristic := D6(3);
        WriteLn('Characteristic=', Characteristic);
    End;
    WriteLn('--------------------------------------------------------------------------------');
End.
