Program First;
Const 
  THOUSAND = 1000;
Var 
  a: Integer;
  b: Integer;
  c: Integer;
Begin
  a := 1;
  {WriteLn('a', '=', a);}
  b := -2;
  {WriteLn('b', '=', b);}
  c := (a - b) * (THOUSAND + THOUSAND) Div THOUSAND;
  WriteLn('c', '=', c);
End.
