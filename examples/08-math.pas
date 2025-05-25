Program Example08Math;
{ With UTF-8 chars for fun! }
Var 
  R: Real;
  I: Integer;
Begin
  WriteLn('PascalScript version ', PS_VERSION, ' (', PS_BITNESS, ' bits)');
  WriteLn;
  WriteLn('===================================');
  WriteLn('| Math constants and functions    |');
  WriteLn('===================================');
  R := EpsReal;
  WriteLn('Epsilon  = ', R);
  R := MinReal;
  WriteLn('Min      = ', R);
  R := MaxReal;
  WriteLn('Max      = ', R);
  R :=  1.23456789E+34;
  WriteLn('1.2E+34  = ', R);
  R :=  1.23456789e-34;
  WriteLn('1.2E-34  = ', R);
  R := Abs(-1/3);
  WriteLn('1/3      = ', R);
  R := Pi;
  WriteLn('π        = ', R);
  I := Trunc(Pi);
  WriteLn('trunc(π) = ', I);
  R := Frac(Pi);
  WriteLn('frac(π)  = ', R);
  R := Round(Pi);
  WriteLn('round(π) = ', R);
  R := Sin(Pi / 4.0);
  WriteLn('sin(π/4) = ', R);
  R := Cos(Pi / 4.0);
  WriteLn('cos(π/4) = ', R);
  R := Tan(Pi / 4.0);
  WriteLn('tan(π/4) = ', R);
  R := ArcTan(1.0);
  WriteLn('atn(1)   = ', R);
  R := Sqr(999.0);
  WriteLn('999²     = ', R);
  R := Sqrt(2.0) / 2.0;
  WriteLn('√2/2     = ', R);
  R := Exp(1.0);
  WriteLn('e        = ', R);
  R := Round(R);
  WriteLn('round(e) = ', R);
  WriteLn('OK!');
End.
