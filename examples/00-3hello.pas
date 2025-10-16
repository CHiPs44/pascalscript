(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
(* Won't work with first versions as they lack strings *)
Program Example003Hello;
Const 
    HelloWorld = '''Hello, ''world''!''';
    H = 'H';
    Hello = 'Hello';
    World = 'world';
Begin
    WriteLn(HelloWorld);
    WriteLn('''Hello, ''world''!''');
    WriteLn(H);
    WriteLn(Hello, ', ', World, '!');
End.
