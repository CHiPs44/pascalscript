program test_fp_not;

type
    t1 = 34..56;
    t2 = (zero, one, two, three);

var
    x, y: t1;
    z: t2;
    t: longint;

begin
    x := low(t1);
    y := pred(x);
    writeln('x=', binstr(x, 32), ' ', x);
    writeln('y=', binstr(y, 32), ' ', y, ' ', low(t1), ' ', y < low(t1));

    x := high(t1);
    y := succ(x);
    writeln('x=', binstr(x, 32), ' ', x);
    writeln('y=', binstr(y, 32), ' ', y);

    x := 42;
    y := not x;
    writeln('x=', binstr(x, 32), ' ', x);
    writeln('y=', binstr(y, 32), ' ', y, ' ', high(t1), ' ', y > high(t1));

    z := two;
    t := not ord(z);
    writeln('z=', z);
    writeln('z=', binstr(ord(z),32));
    writeln('t=', binstr(t, 32));
end.
