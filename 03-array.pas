program arrays;

uses sysutils;

const COUNT = 10;

var numbers: array[0..COUNT - 1] of integer;
var strings: array[0..COUNT - 1] of string;
var i: integer;
var n: integer;

function pad(n: integer; w: integer): string;
var s: string;
begin
    s := inttostr(n);
    while length(s) < w do
    begin
        s := '0' + s;
    end;
    pad := s;
end;

begin
    n := 1;
    for i := 0 to COUNT - 1 do
    begin
        numbers[i] := n;
        strings[i] := '#' + pad(i, 2) + ' ' + inttostr(n);
        n := n * 2;
    end;
    writeln('i  n    s');
    writeln('-- ---- ----------');
    for i := 0 to COUNT - 1 do
    begin
        writeln(pad(i, 2), ' ', numbers[i]:4, ' ', strings[i]);
    end;
end.
