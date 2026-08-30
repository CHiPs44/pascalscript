(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program ExampleProcedure0;

// Var
//     I: Integer;

{ No parameters, with nested procedures }
Procedure Procedure0a;
    Var
        J: Integer;
    Procedure NestedProcedure;
        Procedure InnerNestedProcedure;
        // Var
        //     K: Integer;
        Begin
            // K := 42;
            // WriteLn('            This is InnerNestedProcedure I=', I, ' J=', J);
            WriteLn('            This is InnerNestedProcedure J=', J);
        End;
    Begin
        // WriteLn('        This is NestedProcedure I=', I);
        For J := 1 to 1 Do
            // WriteLn('        This is NestedProcedure I=', I, ' J=', J);
            WriteLn('        This is NestedProcedure J=', J);
            InnerNestedProcedure;
        // WriteLn('        This is NestedProcedure I=', I);
    End;
Begin
    // WriteLn('    This is Procedure0a I=', I);
    J := 123;
    NestedProcedure;
    // WriteLn('    This is Procedure0a I=', I);
End;

{ No parameters with parentheses }
Procedure Procedure0b();
Begin
    // WriteLn('    This is Procedure0b I=', I);
End;

Begin
    // WriteLn('Nested procedures');
    // WriteLn('----------------------------------------------------------------------');
    // I := 0;
    // For I := 1 to 2 Do
    // Begin
        // WriteLn('Begin I=', I);
        Procedure0a;
        // Procedure0b();
        // WriteLn('End I=', I);
    // End;
    // WriteLn('----------------------------------------------------------------------');
End.
