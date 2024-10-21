{ from https://rosettacode.org/wiki/Least_common_multiple#Pascal }

Program RosettaLeastCommonMultiple;

Function LCM(A, B: Cardinal):   Cardinal;
Var
    R: Cardinal;
Begin
    R := A;
    While (R Mod B) <> 0 Do
        R := R + A;
    LCM := R;
End;

Var 
    A, B, R:   Cardinal;
Begin
    A := 12;
    B := 18;
    R := 36;
    WriteLn('LCM(', A, ', ', B, '): ', LCM(A, B), ' (', R, ')');
End.
