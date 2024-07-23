Program TestExpressions;
Const 
  AAA = 1234;
  BBB = -2345;
  CCC = AAA;
  BIN = %01010101;
  OCT = &77777777;
  HEX = $5555AAAA;
Var 
  Test1, Test2: Integer;
  Test3: Boolean;
  Test4: Cardinal;
Begin
  Test1 := AAA + 3 * 456 / 234;
  Test2 := BBB * 3 + CCC;
  Test3 := Test1 * 12 >= Test2 + 3 / 4;
  Test4 := (1 * 2 + 3 * 4) * _HEX_;
  WriteLn(Test1, Test2, Test3, Test4);
End.
