Program Example08Math;
Var
    R: Real;
Begin
    R := MaxReal;
    WriteLn('Max=', R);
    R := Abs(-1.0 / 3.0);
    WriteLn('1/3=', R);
    R := Pi;
    WriteLn('π=', R);
    R := Sin(Pi / 4.0);
    WriteLn('Sin(π/4)=', R);
    R := Cos(Pi / 4.0);
    WriteLn('Cos(π/4)=', R);
    R := ArcTan(1.0);
    WriteLn('Atn(1)=', R);
    R := Sqr(2.0);
    WriteLn('2²=', R);
    R := Sqrt(2.0);
    WriteLn('√2=', R);
    WriteLn('OK!');
End.
