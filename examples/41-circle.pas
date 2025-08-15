(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)

{ from https://www.tutorialspoint.com/pascal/pascal_constants.htm }

Program ConstCircle{(Input, Output)};

// Const Pi = 3.14159265359;

Var Radius, Diameter, Circumference : Real;

Begin
  {WriteLn('Enter the radius of the circle');
  ReadLn(Radius);}
  Radius := 5.0;
  WriteLn(Radius);
  Diameter := Radius * 2.0;
  WriteLn(Diameter);
  Circumference :=  Pi * Diameter;
  WriteLn('The circumference of the circle is ', Circumference{:0:3});
End.
