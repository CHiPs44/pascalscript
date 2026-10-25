(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program StringsHashControl;

Const
  QUOTE = '''';
  // Control characters
  BEL = #7;
  BS = #8;
  HT = #9;
  LF = #10;
  FF = #12;
  CR = #13;
  ESC = #27;
  // Test characters
  K1 = LF;
  K2 = #65;     // 'A'
  K3 = #$42;    // 'B'
  // Test strings
  K4 = 'First'#9'line'#10'Second'#9'line'#10;
  K5 = 'First' ^I 'line' #10 'Second' ^i 'line' #10;

Var
  Dummy: Integer;

Begin
  WriteLn(QUOTE);
//   WriteLn('''', K1, ''' => #', Ord(K1));
//   WriteLn('''', K2, ''' => #', Ord(K2));
//   WriteLn('''', K3, ''' => #', Ord(K3));
//   WriteLn(K4);
//   WriteLn(K5);
End.
