Program SubRange;
// Uses
//   FreePascalCompatibility32;
// not usable by PascalScript yet: {$R+} {$RANGECHECKS ON}
Var
  I: Integer;
  J: 1 .. 10;
  K: Unsigned;
Begin
  I := 5;
  { this won't cause a runtime error because 5 is in the range of J }
  J := I;
  K := 15;
  WriteLn('I = ', I, ' J = ', J, ' K = ', K);
  { this should/will cause a runtime error because 15 is out of the range of J }
  J := K;
  WriteLn('I = ', I, ' J = ', J, ' K = ', K);
End.
