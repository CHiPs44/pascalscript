Program Second;
Const 
    Foo = 1;
    Msg = 'The Quick Brown Fox Jumps Over The Lazy Dog.';
    R = 'The result is: ';
Var
    a, b, c, d : Integer;
Begin
    WriteLn(Msg);
    a := Foo;
    b := 2;
    c := a + b;
    Write(R);
    WriteLn(c);
    d := a * b Div c;
    // Foo := 12;
End.
