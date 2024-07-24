Program second;

Const foo = 1;
  msg = 'The Quick Brown Fox Jumps Over The Lazy Dog.';
  result = 'The result is: ';

Var a, b, c: integer;

Begin
  WriteLn(msg);
  a := foo;
  b := 2;
  c := a + b;
  Write(result);
  WriteLn(c);
  d := a * b Div c;
  foo := 12;
End.
