(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program ExampleProcedure0;

Procedure Procedure0;
Begin
    WriteLn('    This is Procedure0');
End;

Begin
    WriteLn('This is the main program');
    Procedure0;
    WriteLn('This is the main program again');
End.
