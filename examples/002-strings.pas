(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
(* Won't work with first versions *)
Program Example002Hello;
Const
  QUOTE_CHAR      = '''';
  Z_CHAR          = 'Z';
  K               = 'test ''A''';
  EMPTY_STRING    = '';
  HELLO_WORLD_0   = '''Hello, world!''';
  HELLO_WORLD_1   = 'Hello, ''world''!';
  HELLO_WORLD_MAX = 
{
                                                                                                    1         1         1         1         1         1         1         1         1         1         2         2         2         2         2         2
          1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7         8         9         0         1         2         3         4         5
 123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345
}
'123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF'
;
Begin
  WriteLn('Hello, world!');
  WriteLn('Quote: ', QUOTE_CHAR);
  WriteLn('Z    : ', Z_CHAR);
  WriteLn('K    : ', K);
  WriteLn('Empty: ', EMPTY_STRING);
  WriteLn('Zero : ', HELLO_WORLD_0);
  WriteLn('One  : ', HELLO_WORLD_1);
  WriteLn('Max  : ', HELLO_WORLD_MAX);
  WriteLn('Len  : ', Length(HELLO_WORLD_MAX));
  WriteLn('A>B  : ', 'A' > 'B');
  WriteLn('B>A  : ', 'B' > 'A');
  WriteLn('A=A  : ', 'A' = 'A');
  WriteLn('A=B  : ', 'A' = 'B');
  WriteLn('A=a  : ', 'A' = 'a');
End.
