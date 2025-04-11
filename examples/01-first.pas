Program First;
Const 
    THOUSAND = 1000;
Var
    a: Integer;
    b: Integer;
    c: Integer;
    d: Integer;
Begin
    a := +1;
    b := -2;
    {c := a - b + THOUSAND;}
    c := (a - b) * (THOUSAND + THOUSAND) Div THOUSAND;
    d := 6;
    WriteLn('a', '=', a);
    WriteLn('b', '=', b);
    WriteLn('c', '=', c);
    WriteLn('?', '=', c = d);
End.
