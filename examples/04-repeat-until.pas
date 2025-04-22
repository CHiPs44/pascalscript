Program Example04RepeatUntil;
Const
    Loops = 10;
Var
    I : Integer;
Begin
    I := 0;//-Loops;
    Repeat
        Write(I, ' ');
        I := I + 1;
    Until I > Loops;
    WriteLn('O', 'K', '!');
End.
