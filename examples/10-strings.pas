Program StringsExample;
Const 
  HelloWorld = 'Hello, World!';
Var
  s1, s2, s3: String;
Begin
  WriteLn('PascalScript version ', PS_VERSION, ' ', PS_BITNESS, ' bits');
  WriteLn;
  WriteLn('------------------------------');
  WriteLn('String constants and functions');
  WriteLn('------------------------------');
  WriteLn(HelloWorld);
  s1 := 'Hello';
  WriteLn('s1 = ', s1);
  s2 := 'World';
  WriteLn('s2 = ', s2);
  s3 := s1 + s2;
  // s3 := s1 + ' ';
  // s3 := s3 + s2;
  WriteLn('s3 = ', s3);
  // WriteLn('Length(s1) = ', Length(s1));
  // WriteLn('Length(s2) = ', Length(s2));
  // WriteLn('UpperCase(s1) = ', UpperCase(s1));
  // WriteLn('LowerCase(s2) = ', LowerCase(s2));
  // WriteLn('Pos(''lo'', s1) = ', Pos('lo', s1));
  // WriteLn('Pos(''lo'', s2) = ', Pos('lo', s2));
End.
