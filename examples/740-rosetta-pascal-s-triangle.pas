{ from https://rosettacode.org/wiki/Pascal%27s_triangle#Pascal }
Program PascalsTriangle(output);

procedure Pascal(r : Integer);
  var
    i, c, k : Integer;
  begin
    for i := 0 to r-1 do
    begin
      { added by CHiPs44 to make this look like a triangle ;-) }
      if i mod 2 = 0 then
        write('  ');
      for k := 1 to (r div 2) - (i div 2) do
        write('    ');
      { ------------------------------------------------------ }
      c := 1;
      for k := 0 to i do
      begin
        write(c:4);
        c := (c * (i-k)) div (k+1);
      end;
      writeln;
    end;
  end;

begin
  Pascal(9)
end.
