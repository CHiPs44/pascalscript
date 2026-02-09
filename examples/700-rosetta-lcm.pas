{ from https://rosettacode.org/wiki/Least_common_multiple#Pascal }

Program RosettaLeastCommonMultiple;

Const
    Min = 2;
    Max = 9;

Function LCM(A, B: Unsigned):   Unsigned;
Var
    R: Unsigned;
Begin
    R := A;
    While (R Mod B) <> 0 Do
        R := R + A;
    // WriteLn('LCM(', A, ',', B, ') = ', R);
    LCM := R;
    // Result := R;
End;

Var
    A, B, R:   Unsigned;

Begin
    WriteLn('Least Common Multiple (LCM) of all pairs (A,B) with ', Min, '<=A<=', Max, ' and ', Min, '<=B<=', Max);
    WriteLn('--------------------------------------------------------------------------------');
    For A := Min to Max Do
    Begin
        For B := Min to Max Do
        Begin
            If A<>B Then
            Begin
                R := LCM(A, B);
                // if A <   10  Then Write(' ');
                // if B <   10  Then Write(' ');
                Write('(', A:2, ',', B:2, ')=');
                // If R < 1000 Then Write(' ');
                // If R <  100 Then Write(' ');
                // if R <   10 Then Write(' ');
                Write(R:4, ' ');
            End;
        End;
        WriteLn;
    End;
    WriteLn('--------------------------------------------------------------------------------');
End.
{
| A \\ B |    2 |    3 |    4 |    5 |    6 |    7 |    8 |    9 |
| -----: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
|      2 |    2 |    6 |    4 |   10 |    6 |   14 |    8 |   18 |
|      3 |    6 |    3 |   12 |   15 |    6 |   21 |   24 |    9 |
|      4 |    4 |   12 |    4 |   20 |   12 |   28 |    8 |   36 |
|      5 |   10 |   15 |   20 |    5 |   30 |   35 |   40 |   45 |
|      6 |    6 |    6 |   12 |   30 |    6 |   42 |   24 |   18 |
|      7 |   14 |   21 |   28 |   35 |   42 |    7 |   56 |   63 |
|      8 |    8 |   24 |    8 |   40 |   24 |   56 |    8 |   72 |
|      9 |   18 |    9 |   36 |   45 |   18 |   63 |   72 |    9 |
}
