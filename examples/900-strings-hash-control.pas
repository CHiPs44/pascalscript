(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program StringsHashControl;
Const 
  K1 = 'first'#9'line'#10'second'#9'line'#10;
  K2 = 'first'^I'line'#10'second'^i'line'#10;
Begin
  WriteLn(K1);
  WriteLn(K2);
End.
