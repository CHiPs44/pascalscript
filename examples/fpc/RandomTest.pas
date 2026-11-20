program RandomTest;

var
  i, r: LongInt;
  n0, n42: LongInt;
  r64, n64: Int64;
  minR, maxR: LongInt;

begin
  Randomize;

  { Variables, not literals, so the compiler can't fold or reject the calls }
  n0  := 0;
  n42 := -42;

  WriteLn('FPC version: ', {$I %FPCVERSION%});
  WriteLn;

  { Random(0) }
  Write('Random(0) x10        : ');
  for i := 1 to 10 do
    Write(Random(n0), ' ');
  WriteLn;

  { Random(-42): show some samples and the range observed over many calls }
  Write('Random(-42) x10      : ');
  for i := 1 to 10 do
    Write(Random(n42), ' ');
  WriteLn;

  minR := High(LongInt);
  maxR := Low(LongInt);
  for i := 1 to 1000000 do
  begin
    r := Random(n42);
    if r < minR then minR := r;
    if r > maxR then maxR := r;
  end;
  WriteLn('Random(-42) range    : [', minR, ' .. ', maxR, '] over 1e6 calls');

  { Same tests with the Int64 overload }
  n64 := 0;
  r64 := Random(n64);
  WriteLn('Random(Int64(0))     : ', r64);
  n64 := -42;
  Write('Random(Int64(-42))x10: ');
  for i := 1 to 10 do
    Write(Random(n64), ' ');
  WriteLn;

  { Random(1) for comparison: should always be 0 }
  Write('Random(1) x10        : ');
  for i := 1 to 10 do
    Write(Random(1), ' ');
  WriteLn;

  { Random(-1) for comparison: should always be 0 }
  Write('Random(-1) x10       : ');
  for i := 1 to 10 do
    Write(Random(-1), ' ');
  WriteLn;
end.
