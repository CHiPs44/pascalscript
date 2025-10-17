(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example21Procedure1;

Var
    Variable1, I: Integer;
    Variable2: Integer;

{ 2 parameters ot the same type }
Procedure Procedure1(Parameter1, Parameter2: Integer);
Var
    Variable1: Integer; { local variable with the same name as a global variable }
Begin
    Variable1 := Parameter1 * 2 + Parameter2;
    WriteLn('    a. This is Procedure1, Parameter1=', Parameter1, ' Variable1=', Variable1);
End;

{ 1 parameter with the same name as a global variable, another to test if all this works fine }
Procedure Procedure2(Variable2: Integer, Foo: Integer);
Begin
    WriteLn('    1. This is Procedure2, Variable2=', Variable2, ', Foo=', Foo);
    Variable2 := 12 * Foo;
    WriteLn('    2. This is Procedure2, Variable2=', Variable2, ', Foo=', Foo);
End;

Begin
    WriteLn('----------------------------------------------------------------------');
    WriteLn('Procedure with 2 parameters and local variables');
    Variable1 := 1;
    For I := 1 to 3 Do
    Begin
        WriteLn(I, '. This is Program, Variable1=', Variable1, ', Parameter=', I * 10);
        Procedure1(I * 10, 42);
    End;
    WriteLn('----------------------------------------------------------------------');
    WriteLn('Procedure with 2 parameters:');
    WriteLn(' - one having the same name as a global variable,');
    WriteLn(' - another to test if all this works fine');
    Variable2 := 123;
    WriteLn('1. This is Program, Variable2=', Variable2);
    Procedure2(Variable2 * 100, 42);
    WriteLn('2. This is Program, Variable2=', Variable2);
    WriteLn('----------------------------------------------------------------------');
End.
