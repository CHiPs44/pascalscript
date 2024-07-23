program first;
const
  THOUSAND = 1000;
var
  a: integer;
  b, c: integer;
begin
  a := 1;
  b := -2;
  c := (a - b) * (THOUSAND + THOUSAND) div 1000;
  WriteLn(c);
  d := a * b div c;
  foo := 12;
end.
