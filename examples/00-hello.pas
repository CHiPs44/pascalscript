(* Won't work with first versions *)
Program HelloWorld;
Const
  EMPTY_STRING = '';
  (* QUOTE_CHAR = ''''; *)
  ZZZ = 'Z';
  HELLO_WORLD_0 = 'Hello, world!';
  HELLO_WORLD_4 = 'Hello, ''world''!';
  HELLO_WORLD_255 = '0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDE';
  LOREM_IPSUM_1 = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.';
  LOREM_IPSUM_2 = 'Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.';
  LOREM_IPSUM_3 = 'Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.';
  HelloWorld1 = 12345678;
  HelloWorld2 = 123456789023457890123456789023457890;
  HelloWorld3 = 4294967295;
  HelloWorld3 = 2147483648;
  PI = 3.14159265359;
  E = 2.71828182846;
var
  test: integer;
Begin
  test := 1 + 2;
  (*Write(HELLO_WORLD_0);
  WriteLn(HELLO_WORLD_1);
  WriteLn('Hello, world!');*)
End.
