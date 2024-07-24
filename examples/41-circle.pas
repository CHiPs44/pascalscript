{ from https://www.tutorialspoint.com/pascal/pascal_constants.htm }

Program ConstCircle(Input, Output);

Const PI = 3.14159265359;

Var Radius, Diameter, Circumference : Real;

Begin
  WriteLn('Enter the radius of the circle');
  ReadLn(Radius);
  Diameter := 2 * Radius;
  Circumference :=  PI * Diameter;
  WriteLn('The circumference of the circle is ', Circumference:0:3);
End.
