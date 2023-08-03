program first;
const
  foo = 1;
var
  a: integer;
  b: integer;
  c: integer;
begin
  a := foo;
  b := 2;
  c := a + b;
  WriteLn(c);
  d := a * b div c;
  foo := 12;
end.
