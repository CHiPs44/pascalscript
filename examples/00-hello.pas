(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
(* Won't work with first versions *)
Program HelloWorld;
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
'0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF123456789012'
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
End.
