Program BooleanExample;
Const 
  Verbose = True;
  Debug = False;
Var
  i, j: Integer;
  a, b, c, d, e: Boolean;
Begin
  WriteLn('Verbose = ', Verbose);
  WriteLn('Debug   = ', Debug);
  WriteLn(' A   B  And Or  Xor');
  WriteLn('--- --- --- --- ---');
  For i := 0 to 1 do
    For j := 0 to 1 do
    Begin
      If i = 1 Then a := True Else a := False;
      If j = 1 Then b := True Else b := False;
      c := a And b;
      d := a Or b;
      e := a Xor b;
      If a Then Write(' 1  ') Else Write(' 0  ');
      If b Then Write(' 1  ') Else Write(' 0  ');
      If c Then Write(' 1  ') Else Write(' 0  ');
      If d Then Write(' 1  ') Else Write(' 0  ');
      If e Then Write(' 1  ') Else Write(' 0  ');
      WriteLn;
    End;
End.
