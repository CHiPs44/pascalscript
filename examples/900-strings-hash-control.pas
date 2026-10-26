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
  BS = ^H;
  HT = ^I;
  LF = #10;
  FF = #12;
  CR = #13;
  ESC = #27;
  // Test characters
  K1 = LF;
  K2 = #65;        // 'A'
  K3 = #$42;       // 'B'
  K4 = #%01001010; // 'J'
  K5 = #%01101010; // 'j'
  K6 = ^I;
  // Test strings
  K7 = 'First'#9'line'#10'Second'#9'line';
  K8 = 'First' ^I 'line' #10 'Second' ^i 'line';
  K9 = #39 'Dog' #39;

Begin
  WriteLn(QUOTE, K1, QUOTE, ' => #', Ord(K1));
  WriteLn(QUOTE, K2, QUOTE, ' => #', Ord(K2));
  WriteLn(QUOTE, K3, QUOTE, ' => #', Ord(K3));
  WriteLn(QUOTE, K4, QUOTE, ' => #', Ord(K4));
  WriteLn(QUOTE, K5, QUOTE, ' => #', Ord(K5));
  WriteLn(QUOTE, K6, QUOTE, ' => #', Ord(K6));
  WriteLn(QUOTE, K7, QUOTE);
  WriteLn(QUOTE, K8, QUOTE);
  WriteLn(QUOTE, K9, QUOTE);
End.
