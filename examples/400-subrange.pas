Program SubRange;

// Uses
//   FreePascalCompatibility32;

// Not usable by PascalScript yet: {$R-} {$RANGECHECKS OFF}
// Use -r to disable range checking from command line.

Type
  UpperCaseLetter = 'A'..'Z';

Procedure TestInnerType;
Type
  LowerCaseLetter = 'a'..'z';
Var
  L: LowerCaseLetter;
Begin
  L := 'a';
  WriteLn('TestInnerType: L = ''', L, '''');
  // { this should/will cause a runtime error because 'A'' is out of range for 'a'..'z' }
  // L := 'A';
  // WriteLn('L = ''', L, '''');
End;

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
  TestInnerType;
End.
