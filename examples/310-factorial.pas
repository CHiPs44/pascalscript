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

Function IterativeFactorial(N2: Unsigned): Unsigned;
Var
  I, F: Unsigned;
Begin
  If N2 <= 1 Then
    Begin
      If Trace Then
        WriteLn('IterativeFactorial: I=', I, ', F=', F);
      F := 1;
    End
  Else
    Begin
      F := 1;
      For I := 2 To N2 Do
      Begin
        If Trace Then
          WriteLn('IterativeFactorial: N=', N2, ', I=', I, ', F=', F);
        If I > 10 Then
            F := 1 / 0;
        F := F * I;
      End;
    End;
  IterativeFactorial := F;
//   Result := F;
End;

Var
  N, Recursive, Iterative: Unsigned;
  Message: String;

Begin
  WriteLn('Factorial - Recursive and Iterative:');
  WriteLn('--------------------------------------------------------------------------------');
  WriteLn;
  // Repeat
  //   Write('N=');
  //   ReadLn(N);
  // Until N > 0;
  // N := 31;
  WriteLn(' N  Recursive   Iterative');
  WriteLn('--- ---------- ----------');
  For N := 1 To 2 Do
  Begin
    Recursive := 0;//RecursiveFactorial(N);
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
