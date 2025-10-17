(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example04RepeatUntil;
Const
    Loops = 10;
Var
    I : Integer;
Begin
    I := -Loops;
    Repeat
        Write(I, ' ');
        I := I + 1;
    Until I > Loops;
    WriteLn('O', 'K', '!');
End.
