{$R+}
Program TestUnsigned;
Var
    c: Cardinal;
    i: Integer;
Begin
    c := 100000;
    i := -42;
    WriteLn(HexStr(i, 8));
    c := c - i;
    WriteLn(c);
    c := Cardinal(i) - Cardinal(i);
    WriteLn(c);
    c := i - i;
    WriteLn(c);
End.
