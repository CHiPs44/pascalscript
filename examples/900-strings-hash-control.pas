(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program StringsHashControl;

Const
  EMPTY = '';
  HELLO = 'Hello, world!';
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
  // Additional edge cases
  K10 = #0;             // NUL
  K11 = #127;         // DEL
  K12 = 'It''s Pascal'; // Doubled quote
  K13 = #$41 'BC';      // Numeric code followed by text

Begin
  WriteLn('K1: ', QUOTE, K1, QUOTE, ' => #', Ord(K1));
  WriteLn('K2: ', QUOTE, K2, QUOTE, ' => #', Ord(K2));
  WriteLn('K3: ', QUOTE, K3, QUOTE, ' => #', Ord(K3));
  WriteLn('K4: ', QUOTE, K4, QUOTE, ' => #', Ord(K4));
  WriteLn('K5: ', QUOTE, K5, QUOTE, ' => #', Ord(K5));
  WriteLn('K6: ', QUOTE, K6, QUOTE, ' => #', Ord(K6));
  WriteLn('K7: ', QUOTE, K7, QUOTE);
  WriteLn('K8: ', QUOTE, K8, QUOTE);
  WriteLn('K9: ', QUOTE, K9, QUOTE);
  WriteLn('K10: ', QUOTE, K10, QUOTE, ' => #', Ord(K10));
  WriteLn('K11: ', QUOTE, K11, QUOTE, ' => #', Ord(K11));
  WriteLn('K12: ', QUOTE, K12, QUOTE);
  WriteLn('K13: ', QUOTE, K13, QUOTE);
End.
