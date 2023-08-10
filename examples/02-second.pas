program second;

const foo = 1;
      msg = 'The Quick Brown Fox Jumps Over The Lazy Dog.';
      result = 'The result is: ';

var a, b, c: integer;

begin
  WriteLn(msg);
  a := foo;
  b := 2;
  c := a + b;
  Write(result);
  WriteLn(c);
  d := a * b div c;
  foo := 12;
end.
