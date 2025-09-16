(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example20Procedure1;

Var
    Variable1, I, Variable2: Integer;

{ No parameters, with nested procedures }
Procedure Procedure0a;
    Procedure NestedProcedure;
        Procedure InnerNestedProcedure;
        Begin
            WriteLn('            This is InnerNestedProcedure ', I);
        End;
    Begin
        WriteLn('        This is NestedProcedure ', I);
        InnerNestedProcedure;
        WriteLn('        This is NestedProcedure ', I);
    End;
Begin
    WriteLn('    This is Procedure0a ', I);
    NestedProcedure;
    WriteLn('    This is Procedure0a ', I);
End;

{ No parameters with parentheses }
Procedure Procedure0b();
Begin
    WriteLn('    This is Procedure0b ', I);
End;

{ 2 parameters ot the same type }
Procedure Procedure1(Parameter1, Parameter2: Integer);
Var
    Variable1: Integer; { local variable with the same name as a global variable }
Begin
    Variable1 := Parameter1 * 2 + Parameter2;
    WriteLn('    a. This is Procedure1, Parameter1=', Parameter1, 'Variable1=', Variable1);
End;

{ 1 parameter with the same name as a global variable }
Procedure Procedure2(Variable2: Integer);
Begin
    WriteLn('    1. This is Procedure2, Variable2=', Variable2);
    Variable2 := 12;
    WriteLn('    2. This is Procedure2, Variable2=', Variable2);
End;

Begin
    WriteLn('----------------------------------------------------------------------');
    WriteLn('Nested procedures');
    For I := 1 to 2 Do
    Begin
        Procedure0a;
        Procedure0b;
    End;
    WriteLn('Nested procedures');
    WriteLn('----------------------------------------------------------------------');
    Variable1 := 1;
    For I := 1 to 3 Do
    Begin
        WriteLn(I, '. This is Program, Variable1=', Variable1, ', Parameter=', I*10);
        // Procedure1(I*10, 42);
    End;
    WriteLn('----------------------------------------------------------------------');
    Variable2 := 12345;
    WriteLn('1. This is Program, Variable2=', Variable2);
    // Procedure2(Variable2);
    WriteLn('2. This is Program, Variable2=', Variable2);
    WriteLn('----------------------------------------------------------------------');
End.
