{$mode objfpc}

Program TestPowerOperator;

Uses Math;

// Operator **(a, b: Double): Double;
// Begin
//   result := Exp(b * Ln(a));
// End;

Var
  i, j, k: Integer;
//   a, b, c: Double;

Begin
  i := 2;
  j := 3;
  k := Power(i, j); // 2^3 = 8
//   k := i ** j;
  WriteLn(i, ' ** ', j, ' = ', k);
//   a := 2.0;
//   b := 3.0;
//   // c := Power(a, b); // 2^3 = 8
//   c := a ** b;
//   WriteLn(a:0, ' ** ', b:0, ' = ', c:0);
End.
