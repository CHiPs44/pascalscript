Program BooleanExample;
Const
    Verbose = True;
Var
  a, b, c: Boolean;
Begin
    WriteLn('Verbose = ', Verbose);
    a:= True;
    b:= False;
    c:= a And b;
    WriteLn('a And b = ', c);
    c:= a Or b;
    WriteLn('a Or  b = ', c);
End.
