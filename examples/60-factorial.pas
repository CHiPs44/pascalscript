Program Factorial;

Const
  Trace = False;

Function RecursiveFactorial(N: Unsigned): Unsigned;
Begin
  If N <= 1 Then
    Begin
      Result := 1;
      If Trace Then
        WriteLn('RecursiveFactorial(', N, ') = ', Result);
    End
  Else
    Begin
      Result := N * RecursiveFactorial(N -1);
      If Trace Then
        WriteLn('RecursiveFactorial(', N, ') = ', Result);
    End;
End;

Function IterativeFactorial(N: Unsigned): Unsigned;
Var
  I, F: Unsigned;
Begin
  If N <= 1 Then
    Begin
      If Trace Then
        WriteLn('IterativeFactorial: I=', I, ', F=', F);
      F := 1;
    End
  Else
    Begin
      F := 1;
      For I := 2 To N Do
      Begin
        If Trace Then
          WriteLn('IterativeFactorial: I=', I, ', F=', F);
        F := F * I;
      End;
    End;
  // IterativeFactorial := F;
  Result := F;
End;

Var
  N, Recursive, iterative: Unsigned;
Begin
  // Repeat
  //   Write('N=');
  //   ReadLn(N);
  // Until N > 0;
  // N := 31;
  WriteLn('Factorial:');
  For N := 0 To 15 Do
  Begin
    Recursive := RecursiveFactorial(N);
    // WriteLn(' - Recursive: ', N, '! = ', Recursive);
    iterative := IterativeFactorial(N);
    // WriteLn(' - Iterative: ', N, '! = ', iterative);
    WriteLn(' - ', N, '! = ', Recursive, ' = ', iterative);
  End;
End.
