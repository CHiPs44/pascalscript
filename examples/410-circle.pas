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
  WriteLn('Enter the radius of the circle');
  ReadLn(Radius);
  WriteLn('The radius of the circle is ', Radius:0:3);
  Diameter := Radius * 2.0;
  WriteLn('The diameter of the circle is ', Diameter:0:3);
  Circumference :=  Pi * Diameter;
  WriteLn('The circumference of the circle is ', Circumference:0:3);
End.
