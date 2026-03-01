(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program TruthTable;

Const
    Yes = True;
    No = False;

Var
    I, J: Integer;
    A, B, C, D, E: Boolean;

Begin
    WriteLn('Booleans and truth table'                                                        );
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('Yes = ', Yes);
    WriteLn('No  = ', No  );
    WriteLn(' A   B  And Or  Xor');
    WriteLn('--- --- --- --- ---');
    For I := 0 To 1 Do
        For J := 0 To 1 Do
        Begin
            If I = 1 Then A := True Else A := False;
            If J = 1 Then B := True Else B := False;
            C := A And B;
            D := A Or  B;
            E := A Xor B;
            If A Then Write(' 1  ') Else Write(' 0  ');
            If B Then Write(' 1  ') Else Write(' 0  ');
            If C Then Write(' 1  ') Else Write(' 0  ');
            If D Then Write(' 1  ') Else Write(' 0  ');
            If E Then Write(' 1  ') Else Write(' 0  ');
            WriteLn;
        End;
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('OK!'                                                                             );
End.
