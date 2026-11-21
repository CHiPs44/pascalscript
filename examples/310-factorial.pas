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
      Result := N * RecursiveFactorial(N - 1);
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
          WriteLn('IterativeFactorial: N=', N, ', I=', I, ', F=', F);
        If I > 100 Then
            F := 1 / 0;
        F := F * I;
      End;
    End;
  IterativeFactorial := F;
//   Result := F;
End;

Var
  Recursive, Iterative: Unsigned;
  N: Unsigned;
  Message: String;

Begin
  WriteLn('Factorial - Recursive and Iterative:');
  WriteLn('--------------------------------------------------------------------------------');
  // Repeat
  //   Write('N=');
  //   ReadLn(N);
  // Until N > 0;
  // N := 13;
  WriteLn(' N  Recursive   Iterative');
  WriteLn('--- ---------- ----------');
  //       123 1234567890 1234567890
  For N := 0 To 13 Do
  Begin
    Recursive := RecursiveFactorial(N);
    Iterative := IterativeFactorial(N);
    If Recursive <> Iterative Then
      Message := 'FAILED!'
    Else
      Message := 'OK!';
    WriteLn(N:2, '! ', Recursive:10, ' ', Iterative:10, ' ', Message);
  End;
  WriteLn('--------------------------------------------------------------------------------');
  WriteLn('OK!');
End.
