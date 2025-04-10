Program First;
Const 
    THOUSAND = 1000;
Var
    a: Integer;
    b: Integer;
    c: Integer;
Begin
    a := +1;
    b := -2;
    {c := a - b + THOUSAND;}
    c := (a - b) * (THOUSAND + THOUSAND) Div THOUSAND;
    WriteLn('a', '=', a);
    WriteLn('b', '=', b);
    WriteLn('c', '=', c);
End.
