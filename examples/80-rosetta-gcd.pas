{ from https://rosettacode.org/wiki/Greatest_common_divisor#Pascal_/_Delphi_/_Free_Pascal }

Program RosettaGreatestCommonDivisor;

Function RecursiveGCD(U, V: Integer):   Integer;
Var 
    T: Integer;
Begin
    // WriteLn('    1. GCD(', U, ', ', V, ') = ?');
    If V = 0 Then
    Begin
        Result := U;
        // WriteLn('    2. GCD(', U, ', ', V, ') = ', Result);
    End
    Else
    Begin
        T := U Mod V;
        // WriteLn('    3. GCD(', U, ', ', V, ') = GCD(',V, ', ', T, ')');
        Result := RecursiveGCD(V, T);
        // WriteLn('    4. GCD(', U, ', ', V, ') = ', Result);
    End;
End;

Function IterativeGCD(U, V: Integer):   Integer;
Var 
    T: Integer;
Begin
    // WriteLn('    1. GCD(', U, ', ', V, ') = ?');
    While V <> 0 Do
        Begin
            T := U;
            U := V;
            V := T Mod V;
        End;
    // IterativeGCD := U;
    Result := Abs(U);
End;

Var 
    U, V, R1, R2, R3: Integer;
Begin
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('Greatest Common Divisor (GCD)');
    U := 48; V := 18; R1 := 6;
    // U := 231; V := 7; R1:= 7;
    // U := 333; V := 34; R1:= 7;
    // U := 69811; V := 49865; R1 := 9973;
    R2 := RecursiveGCD(U, V);
    R3 := IterativeGCD(U, V);
    WriteLn('GCD(', U, ', ', V, '): ', R2, ' (', R1, ', recursive)');
    WriteLn('GCD(', U, ', ', V, '): ', R3, ' (', R1, ', iterative)');
End.
