(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)

Program Example300Function0;

Function FooBar(Baz: Unsigned): Unsigned;
Begin
  // FooBar := Baz * 2;
  Result := Baz * 2;
End;

Procedure Dummy;
Begin
    // Do nothing
End;

Var 
  I, J: Unsigned;

Begin
  I := 21;
  WriteLn('Calling FooBar with ', I, '...');
  J := FooBar(I);
  WriteLn('FooBar returned ', J);
  // KO for now, but should be OK in the future:
  // WriteLn(FooBar(11223344), '*STOP*');
End.
