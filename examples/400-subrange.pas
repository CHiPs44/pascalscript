Program SubRange;
// Uses
//   FreePascalCompatibility32;
// not usable by PascalScript yet: {$R+} {$RANGECHECKS ON}
Type
  UpperCaseLetter = 'A'..'Z';
Var
  I: Integer;
  J: 1 .. 10;
  K: Unsigned;
  L: UpperCaseLetter;
Begin
  I := 5;
  { this won't cause a runtime error because 5 is in 1..10 }
  J := I;
  WriteLn('I = ', I, ' J = ', J);
  // K := 15;
  // { this should/will cause a runtime error because 15 is out of range for 1..10 }
  // J := K;
  // WriteLn('I = ', I, ' J = ', J, ' K = ', K);
  L := 'L';
  WriteLn('L = ''', L, '''');
  // // { this should/will cause a runtime error because 'a'' is out of range for 'A'..'Z' }
  // L := 'a';
  // WriteLn('L = ''', L, '''');
End.
