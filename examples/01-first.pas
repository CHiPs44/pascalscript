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
  c := (a + b) * (foo + 2) / 1000;
  WriteLn(c);
  d := a * b div c;
  foo := 12;
end.
