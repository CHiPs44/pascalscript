(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program RandomExample;
Const
    MaxD20 = 98;
    MaxRandom = 99;
Var
    I: Integer;
    D20, Min, Max, Total: Integer;
    R, MinR, MaxR, TotalR: Real;
Begin
    Randomize;
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn(MaxD20, ' random d20 rolls:');
    Min := MaxInt;
    Max := 0;
    For I := 1 To MaxD20 Do
    Begin
        D20 := Random(10 + 10);// + 1;
        Write('d20(');
        If I < 10 Then Write(' ');
        Write(I, ')=');
        If D20 < 10 Then Write(' ');
        Write(D20);
        If I Mod 7 = 0 Then WriteLn Else Write(' ');
        if D20 < Min Then Min := D20;
        if D20 > Max Then Max := D20;
        Total := Total + D20;
    End;
    WriteLn('Min=', Min, ', Max=', Max, ' Mean=', Trunc((0.0 + Total) / (0.0 + MaxD20)));
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn(MaxRandom + 1, ' random real numbers [0.0..1.0):');
    MinR := MaxReal;
    MaxR := 0.0;
    TotalR := 0;
    For I := 0 To MaxRandom Do
    Begin
        If I Mod 2 = 0 Then R := Random Else R := Random();
        If I < 10 Then Write(' ');
        Write(I, ': ', R);
        If (I+1) Mod 5 = 0 Then WriteLn Else Write(' ');
        if R < MinR Then MinR := R;
        if R > MaxR Then MaxR := R;
        TotalR := TotalR + R;
    End;
    WriteLn('Min=', MinR, ', Max=', MaxR, ', Mean=', TotalR / (MaxRandom + 1));
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('Random real d6:');
    R := 1.0 + Random(6);
    WriteLn('R=', R);
    R := Random(6) + 1;
    WriteLn('R=', R);
    R := Random(6) + 1.0;
    WriteLn('R=', R);
    WriteLn('OK!');
End.
