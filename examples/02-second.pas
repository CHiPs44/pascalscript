Program Second;
Const 
    Foo = 100;
Var
    a, b, c, d: Integer;
    r: Real;
Begin
    a := Foo;
    WriteLn('a=', a);
    b := 20;
    WriteLn('b=', b);
    c := 234;
    c := a + b + c;
    WriteLn('c=', c);
    d := (a * b) Div c;
    WriteLn('d=', d);
    r := (a * b) / c;
    WriteLn('r=', r);
    // Foo := 12;
End.
