Program TestExpressions;

Const
  AAA = 1234;
  BBB = -2345;
  CCC = AAA;
  BIN = %01010101;
  OCT = &77777777;
  HEX = $5555AAAA;

Var
  Test1: Integer;
  Test2: Integer;
  Test3: Boolean;
  Test4: Unsigned;

Begin
  Test1 := AAA + 3 * 456 Div 234;
  Test2 := BBB * 3 + CCC + 3 Div 4;
  Test3 := Test1 * 12 >= Test2;
  Test4 := (1 * 2 + 3 * 4) * HEX;
  WriteLn(Test1);
  WriteLn(Test2);
  WriteLn(Test3);
  WriteLn(Test4);
End.
