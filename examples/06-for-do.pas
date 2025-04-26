Program Example05WhileDo;
Const
    Limit = 5;
Var
    I : Integer;
Begin
    For I := -Limit To Limit Do
    Begin
        Write(I);
        Write(' ');
    End;
    WriteLn('O', 'K', '!');
End.
