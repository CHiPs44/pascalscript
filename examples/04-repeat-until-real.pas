Program Example04RepeatUntilReal;
Const
    Limit = 1.0;
Var
    R : Real;
Begin
    R := -Limit;
    Repeat
        Write(R, ' ');
        R := R + 0.1;
    Until R - 0.01 > Limit;
    WriteLn('O', 'K', '!');
End.
