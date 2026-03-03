{ from https://rosettacode.org/wiki/Day_of_the_week#Pascal }
program dayoftheweek(output);

var
  y: integer;
  d: integer;

  function dayofweek(y, m, d: integer): integer;
    (* Sunday = 0, Saturday = 6 *)
  var
    z: integer;
  begin
    if m < 3 then
    begin
      y := y - 1;
      m := m + 12;
    end;
    z := y + (y div 4) - (y div 100) + (y div 400);
    dayofweek := (z + d + (153 * m + 8) div 5) mod 7;
  end;

begin
  for y := 2007 to 2122 do
  begin
    // works with intermediate variable
    d := dayofweek(y, 12, 25);
    // ERROR Cannot convert value from type 'INTEGER' (based on 'INTEGER') to type 'BOOLEAN' (based on 'BOOLEAN')
    // if dayofweek(y, 12, 25) = 0 then
    if d = 0 then
      write(y:5);
  end;
  writeln
end.
