Program Example04RepeatUntil;
Const
    Loops = 10;
Var
    I : Integer;
Begin
    I := Loops;
    Repeat
        Write(I, ' ');
        I := I - 1;
    Until I < 0;
    WriteLn('O', 'K', '!');
End.
