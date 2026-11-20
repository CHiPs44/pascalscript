(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program RandomExample2;

Var
    I: Integer;
    Z: Integer;
    Sum, Min, Max: Integer;

Begin
    WriteLn('PascalScript Random Number Generation Example #2');
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('Seeding Random Number Generator...');
    Randomize;

    WriteLn('--------------------------------------------------------------------------------');
    Write('Random(0) x10        : ');
    Sum := 0;
    For i := 1 To 10 Do
    Begin
        Z := Random(0);
        Write(Z, ' ');
        Sum := Sum + Z;
    End;
    Write(' => ', Sum);
    If Sum=0 Then
        WriteLn(' OK!')
    Else
        WriteLn(' FAILED!');

    WriteLn('--------------------------------------------------------------------------------');
    Write('Random(1) x10        : ');
    Sum := 0;
    For i := 1 To 10 Do
    Begin
        Z := Random(1);
        Write(Z, ' ');
        Sum := Sum + Z;
    End;
    Write(' => ', Sum);
    If Sum=0 Then
        WriteLn(' OK!')
    Else
        WriteLn(' FAILED!');

    WriteLn('--------------------------------------------------------------------------------');
    Write('Random(-1) x10       : ');
    Sum := 0;
    For i := 1 To 10 Do
    Begin
        Z := Random(-1);
        Write(Z, ' ');
        Sum := Sum + Z;
    End;
    Write(' => ', Sum);
    If Sum=0 Then
        WriteLn(' OK!')
    Else
        WriteLn(' FAILED!');

    WriteLn('--------------------------------------------------------------------------------');
    Write('Random(-42) x10      : ');
    Min := MaxInt;
    Max := MinInt;
    For i := 1 To 10 Do
    Begin
        Z := Random(-42);
        Write(Z, ' ');
        If Z < Min Then Min := Z;
        If Z > Max Then Max := Z;
    End;
    Write(' => ', Min, ' ', Max);
    If Min <= Max Then
        WriteLn(' OK!')
    Else
        WriteLn(' FAILED!');

    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('OK!');
End.
