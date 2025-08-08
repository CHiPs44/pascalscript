(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example03IfThenElse;
Var 
  I : Integer;
Begin
  I := 1;
    { No else }
  If I = 1 Then
    WriteLn('I', '=', '1', ' ', '(' , I, ')');
    { No else with compound statement }
  If I = 1 Then
    Begin
      Write('I', '=', '1');
      WriteLn(' ', '(' , I, ')');
    End;
    { One statement in then and else }
  If I = 1 Then
    Write('I', '=', '1') { No ; }
  Else
    Write('I', '<', '>', '1');
  WriteLn(' ', '(' , I, ')');
  I := (I + 1) * 1000;
    { One compound statement in then and else }
  If I = 1 Then
    Begin
      Write('I');
      Write('=');
      Write('1');
    End
  Else
    Begin
      Write('I');
      Write('<');
      Write('>');
      Write('1');
    End;
  WriteLn(' ', '(' , I, ')');
  I := 2;
  If I + 1 = 2 Then
    If I + 2 = 3 Then
      WriteLn('I', '=', '1', ' ', '(' , I, ')')
  Else
    WriteLn('I', '<', '>', '1', ' ', '(' , I, ')')
  Else
    WriteLn('I', '<', '>', '1', ' ', '(' , I, ')')
End.
