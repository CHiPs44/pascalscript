(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example300Function0;

// Function FooBar(Baz: Integer): Integer;
// Begin
//     WriteLn('   FooBar: Baz=', Baz);
//     FooBar := Baz * 2;
//     Result := Baz * 2;
//     WriteLn('   FooBar: Result=', Result);
// End;

Procedure Toto(Titi: Integer);
Begin
    WriteLn('Toto called with', Titi);
End;

Function FooBar(Baz: Integer): Integer;
Begin
    FooBar := Baz * 2;
End;

Var
    I, J: Integer;

    // I := 21;
    // WriteLn('Calling FooBar with ', I, '...');
    // J := FooBar(I);
    // WriteLn(' FooBar returned ', J);
    // WriteLn(' FooBar returned ', FooBar(I), ' for input ', I);
    // I := 21;

Begin
    Toto(1);
    WriteLn('Calling FooBar with 21...');
    I := 21;
    J := FooBar(I);
    WriteLn(J);
    // WriteLn(' FooBar returned ', J);
    // WriteLn(FooBar(21), '*STOP*');
End.
