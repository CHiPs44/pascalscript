program second;

const foo = 1;
const msg = 'The Quick Brown Fox Jumps Over The Lazy Dog.';
const result = 'The result is: ';

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
