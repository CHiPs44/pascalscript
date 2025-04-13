Program First;
Const 
    THOUSAND = 1000;
Var
    a, b, c, d: Integer;
Begin
    a := +1;
    b := -2;
    c := (a - b) * (THOUSAND + THOUSAND) Div THOUSAND;
    d := 8 * a + b;
    WriteLn('a', '=', a);
    WriteLn('b', '=', b);
    WriteLn('c', '=', c);
    WriteLn('d', '=', d);
    WriteLn('c', '=', 'd', '?', ' ', c = d);
End.
