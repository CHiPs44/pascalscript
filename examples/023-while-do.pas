Program WhileDo;

Const
    Loops = 10;

Var
    I : Integer;

Begin
    WriteLn('Example 040: Repeat-Until Loop with Integer numbers');
    WriteLn('--------------------------------------------------------------------------------');
    I := Loops;
    While I >= 0 Do
    Begin
        Write(I, ' ');
        I := I - 1;
    End;
    WriteLn;
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('OK!');
End.
