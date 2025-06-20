(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example07Random;
Var
    I: Integer;
    D20, Min, Max: Integer;
    R, MinR, MaxR: Real;
Begin
    Randomize;
    WriteLn('--------------------------------------------------------------------------------');
    Min := MaxInt;
    Max := 0;
    For I := 0 To 29 Do
    Begin
        D20 := Random(10 + 10) + 1;
        Write('d20(');
        If I < 10 Then Write(' ');
        Write(I, ')=');
        If D20 < 10 Then Write(' ');
        Write(D20);
        If (I+1) Mod 5 = 0 Then WriteLn Else Write(' ');
        if D20 < Min Then Min := D20;
        if D20 > Max Then Max := D20;
    End;
    WriteLn('Min=', Min, ', Max=', Max);
    WriteLn('--------------------------------------------------------------------------------');
    MinR := MaxReal;
    MaxR := 0.0;
    For I := 0 To 99 Do
    Begin
        If I Mod 2 = 0 Then
            R := Random
        Else
            R := Random();
        Write('R(', I, ')=', R);
        If (I+1) Mod 5 = 0 Then WriteLn Else Write(Chr(9));
        if R < MinR Then MinR := R;
        if R > MaxR Then MaxR := R;
    End;
    WriteLn('Min=', MinR, ', Max=', MaxR);
    WriteLn('--------------------------------------------------------------------------------');
    // Note: Integer is now cast to Real automatically
    R := Random(6) + 1;
    WriteLn('R=', R);
    WriteLn('OK!');
End.
