(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)

Program Example300Function0;

Function FooBar(Baz: Integer): Integer;
Begin
    WriteLn(' FooBar: Baz=', Baz);
    // FooBar := Baz * 2;
    Result := Baz * 2;
End;

Var
    I, J: Integer;

Begin
    I := 21;
    // WriteLn('Calling FooBar with ', I, '...');
    // J := FooBar(I);
    // WriteLn(' FooBar returned ', J);
    WriteLn(' FooBar returned ', FooBar(I));
End.
