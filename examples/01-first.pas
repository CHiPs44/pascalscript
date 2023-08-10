program first;
const
  foo = 1000;
var
  a: integer;
  b: integer;
  c: integer;
begin
  a := 1;
  b := 2;
  c := (a + b) * (foo + foo) / 1000;
  WriteLn(c);
  d := a * b div c;
  foo := 12;
end.
