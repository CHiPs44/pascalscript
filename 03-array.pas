program arrays;

const COUNT = 20;

var Numbers: array[0..COUNT - 1] of LongInt;
var Strings: array[0..COUNT - 1] of string;
var i: integer;
var n: LongInt;

{ Same as Free Pascal SysUtils }
function Int2Str(number: LongInt): string;
var s: string;
var digit: integer;
begin
    if number = 0 then
    begin
        s := '0';
    end
    else
    begin
        if number < 0 then
        begin
            number := -number;
        end;
        s := '';
        while number > 0 do
        begin
            digit := number mod 10;
            number := number div 10;
            s := Chr(Ord('0') + digit) + s;
        end;
    end;
    Int2Str := s;
end;

function Pad(n: LongInt; w: integer): string;
var s: string;
begin
    s := Int2Str(n);
    while Length(s) < w do
    begin
        s := '0' + s;
    end;
    Pad := s;
end;

begin
    n := 1;
    for i := 0 to COUNT - 1 do
    begin
        Numbers[i] := n;
        Strings[i] := '#' + Pad(i, 2) + ' ' + Int2Str(n);
        n := n * 2;
    end;
    WriteLn('#  | Number | String    ');
    WriteLn('---|--------|-----------');
    for i := 0 to COUNT - 1 do
    begin
        WriteLn(Pad(i, 2), ' | ', Numbers[i]:6, ' | ', Strings[i]);
    end;
end.
