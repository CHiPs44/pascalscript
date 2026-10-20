(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program StringsHashControl;
Const
//   K1 = 'first'#9'line'#10'second'#9'line'#10;
//   K2 = 'first'^I'line'#10'second'^i'line'#10;
  K1 = ^M;
  K2 = #65;
  K3 = #$42;
Begin
  WriteLn(Ord(K1));
  WriteLn(K2);
  WriteLn(K3);
End.
