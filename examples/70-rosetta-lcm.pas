{ from https://rosettacode.org/wiki/Least_common_multiple#Pascal }

Program RosettaLeastCommonMultiple;

Function LCM(A, B: Unsigned):   Unsigned;
Var
    R: Unsigned;
Begin
    R := A;
    While (R Mod B) <> 0 Do
        R := R + A;
    // LCM := R;
    Result := R;
End;

Const
    Min = 2;
    Max = 9;
Var 
    A, B, R:   Unsigned;
Begin
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('Least Common Multiple (LCM) of all pairs (A,B) with ', Min, '<=A<=', Max, ' and ', Min, '<=B<=', Max);
    For A := Min to Max Do
    Begin
        For B := Min to Max Do
        Begin
            If A<>B Then
            Begin
                R := LCM(A, B);
                if A <   10  Then Write(' ');
                if B <   10  Then Write(' ');
                Write('(', A, ',', B, ')=');
                If R < 1000 Then Write(' ');
                If R <  100 Then Write(' ');
                if R <   10 Then Write(' ');
                Write(R, ' ');
            End;
        End;
        WriteLn;
    End;
    WriteLn('--------------------------------------------------------------------------------');
End.
