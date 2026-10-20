(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program StringsHashControl;
Const
  BEL = #7;
  BS = #8;
  HT = #9;
  LF = #10;
  FF = #12;
  CR = #13;
  ESC = #27;
//   K1 = 'first'#9'line'#10'second'#9'line'#10;
//   K2 = 'first' ^I 'line' #10 'second' ^i 'line' #10;
  K1 = LF;
  K2 = #65;
  K3 = #$42;
Begin
  WriteLn('''', K1, ''' => #', Ord(K1));
  WriteLn('''', K2, ''' => #', Ord(K2));
  WriteLn('''', K3, ''' => #', Ord(K3));
End.
