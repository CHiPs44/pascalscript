(*
        This file is part of the PascalScript Pascal interpreter.
        SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
        SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program TypeDef0;

Type
    MyInteger1 = Integer;
    MyInteger2 = MyInteger1;

Var
    I : MyInteger1;
    J : MyInteger2;
    K : Integer;

Begin
    I := 42;
    Write(I, ' <= 42 expected: '); If I = 42 Then WriteLn('OK') Else WriteLn('KO');
    J := 43;
    Write(J, ' <= 43 expected: '); If J = 43 Then WriteLn('OK') Else WriteLn('KO');
    K := J + 1;
    Write(K, ' <= 44 expected: '); If K = 44 Then WriteLn('OK') Else WriteLn('KO');
End.
