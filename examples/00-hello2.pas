Program HelloWorld;
Const 
  AAA = 1234;
  BBB = -2345;
  _TEST_ = %01010101;
Var 
  Test1, Test2: Integer;
  Test3: Boolean;
  Test4: Cardinal;
Begin
  Test1 := AAA + 3 * 4;
  Test2 := AAA * 3 + 4;
  Test3 := Test1 * 12 >= Test2 + 3 / 4;
  Test4 := (1 * 2 + 3 * 4) * _TEST_;
End.
