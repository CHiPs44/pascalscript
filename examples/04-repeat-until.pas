Program Example04RepeatUntil;
Const
    Three = 3;
Var
    I : Integer;
Begin
    I := 1;
    Repeat
        WriteLn(I);
        I := I + 1;
    Until I > Three;
End.
