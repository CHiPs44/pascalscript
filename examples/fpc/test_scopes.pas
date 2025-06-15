Program TestScopes;

Var X : Real;

Procedure Test1;
Var X : Integer;
Begin
  X := 1; // This refers to the local variable X of type Integer
  WriteLn('Local X: ', X); // Should output 1
  WriteLn('Global X: ', TestScopes.X); // Should output 2
End;

Begin
  X := 2; // This refers to the global variable X of type Real
  Test1;
  WriteLn('Global X: ', X); // Should output 2
End.
