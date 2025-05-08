Program Example08Math;
{ With UTF-8 chars for fun! }
Var
    R: Real;
Begin
    WriteLn('PascalScript version ', PS_VERSION);
    WriteLn;
    WriteLn('Math constants and functions');
    WriteLn('----------------------------');
    R := MaxReal;
    WriteLn('Max      = ', R);
    R := Abs(-1.0 / 3.0);
    WriteLn('1/3      = ', R);
    R := Pi;
    WriteLn('π        = ', R);
    R := Sin(Pi / 4.0);
    WriteLn('sin(π/4) = ', R);
    R := Cos(Pi / 4.0);
    WriteLn('cos(π/4) = ', R);
    R := ArcTan(1.0);
    WriteLn('atn(1)   = ', R);
    R := Sqr(9.0);
    WriteLn('9²       = ', R);
    R := Sqrt(2.0) / 2.0;
    WriteLn('√2/2     = ', R);
    WriteLn('OK!');
End.
