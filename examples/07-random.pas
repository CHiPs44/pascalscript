Program Example07Random;
Var
    I: Integer;
    D20, Min, Max: Integer;
    R: Real;
Begin
    Randomize;
    Min := MaxInt;
    Max := 0;
    For I := 0 To 9 Do
    Begin
        D20 := Random(10 + 10) + 1;
        WriteLn('d', '2', '0', '(', I, ')', '=', D20);
        if D20 < Min Then Min := D20;
        if D20 > Max Then Max := D20;
    End;
    WriteLn('M', 'i', 'n', '=', Min);
    WriteLn('M', 'a', 'x', '=', Max);
    For I := 0 To 9 Do
    Begin
        // KO: If I Mod 2 = 0 Then
        If I < 5 Then
            R := Random
        Else
            R := Random();
        WriteLn('R', '(', I, ')', '=', R);
    End;
    // Note: Integer is now cast to Real automatically
    R := Random(6) + 1;
    WriteLn('R', '=', R);
    WriteLn('O', 'K', '!');
End.
