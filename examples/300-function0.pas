(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)

Program Example300Function0;

Function FooBar(Baz: Unsigned): Unsigned;
Begin
  // WriteLn('   FooBar: Baz=', Baz);
  FooBar := Baz * 2;
  // Result := Baz * 2;
  // WriteLn('   FooBar: Result=', Result);
End;

// Procedure Dummy;
// Begin
//     // Do nothing
// End;

Var 
  I, J: Unsigned;

Begin
  // WriteLn('Calling FooBar with 21...');
  // I := 21;
  J := FooBar(I);
  WriteLn('FooBar returned ', J);
  // WriteLn(FooBar(21), '*STOP*');
End.
