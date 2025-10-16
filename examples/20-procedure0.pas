(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example20Procedure0;

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

Begin
    WriteLn('Nested procedures');
    WriteLn('----------------------------------------------------------------------');
    For I := 1 to 2 Do
    Begin
        WriteLn('Begin ', I);
        Procedure0a;
        Procedure0b();
        WriteLn('End ', I);
    End;
    WriteLn('----------------------------------------------------------------------');
End.
