Program First;
Const 
  THOUSAND = 1000;
Var 
  a: Integer;
  b, c: Integer;
Begin
  a := 1;
  b := -2;
  c := (a - b) * (THOUSAND + THOUSAND) Div THOUSAND;
  WriteLn(c);
End.
