(* Won't work with first versions *)
Program HelloWorld;
Const
  QUOTE_CHAR = '''';
  Z_CHAR = 'Z';
  EMPTY_STRING = '';
  HELLO_WORLD_0 = 'Hello, world!';
  HELLO_WORLD_1 = 'Hello, ''world''!';
  HELLO_WORLD_MAX = {
                                                                                                   1         1         1         1         1         1         1         1         1         1         2         2         2         2         2         2
         1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7         8         9         0         1         2         3         4         5
12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
}
'0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF123456789012'
;
Begin
  WriteLn('Hello, world!');
  WriteLn(QUOTE_CHAR);
  WriteLn(Z_CHAR);
  WriteLn(EMPTY_STRING);
  WriteLn(HELLO_WORLD_0);
  WriteLn(HELLO_WORLD_1);
  WriteLn(HELLO_WORLD_MAX);
End.
