
Program Factorial;

Function RecursiveFactorial(N: Integer): Integer;
Begin
  If N <= 1 Then
    Begin
      RecursiveFactorial := 1;
      (*WriteLn('RecursiveFactorial(', N, ') = ', RecursiveFactorial);*)
    End
  Else
    Begin
      RecursiveFactorial := N * RecursiveFactorial(N -1);
      (*WriteLn('RecursiveFactorial(', N, ') = ', RecursiveFactorial);*)
    End;
End;

Function IterativeFactorial(N: Integer): Integer;
Var 
  I, F: Integer;
Begin
  If N <= 1 Then
    Begin
      (*WriteLn('IterativeFactorial: I=', I, ', F=', F);*)
        F := 1;
    End
  Else
    Begin
      F := 1;
      For I := 2 To N Do
      Begin
        (*WriteLn('IterativeFactorial: I=', I, ', F=', F);*)
        F := F * I;
      End;
    End;
  IterativeFactorial := F;
End;

Var 
  N: Integer;
Begin
  Repeat
    Write('N=');
    ReadLn(N);
  Until N > 0;
  WriteLn('Recursive: ', N, '! = ', RecursiveFactorial(N));
  WriteLn('Iterative: ', N, '! = ', IterativeFactorial(N));
End.
