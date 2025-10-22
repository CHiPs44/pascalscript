Program WhileDo;
Const
    Loops = 10;
Var
    I : Integer;
Begin
    I := Loops;
    While I >= 0 Do
    Begin
        Write(I, ' ');
        I := I - 1;
    End;
    WriteLn('O', 'K', '!');
End.
