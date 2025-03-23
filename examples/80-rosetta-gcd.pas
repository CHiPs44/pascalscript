{ from https://rosettacode.org/wiki/Greatest_common_divisor#Pascal_/_Delphi_/_Free_Pascal }

Program RosettaGreatestCommonDivisor;

Function RecursiveGCD(U, V: UNSIGNED):   UNSIGNED;
Begin
    If V = 0 Then
        RecursiveGCD := U
    Else
        RecursiveGCD := RecursiveGCD(V, U Mod V) ;
End;

Function IterativeGCD(U, V: UNSIGNED):   UNSIGNED;
Var 
    T:   UNSIGNED;
Begin
    While V <> 0 Do
        Begin
            T := U;
            U := V;
            V := T Mod V;
        End;
    IterativeGCD := U;
End;

Var 
    U, V, R:   UNSIGNED;
Begin
    {U := 231;
    V := 7;
    R:= 7;}
    U := 49865;
    V := 69811;
    R := 9973;
    WriteLn('GCD(', U, ', ', V, '): ', RecursiveGCD(U, V), ' (', R, ', recursive)');
    WriteLn('GCD(', U, ', ', V, '): ', IterativeGCD(U, V), ' (', R, ', iterative)');
End.
