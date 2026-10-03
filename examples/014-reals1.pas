(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    WriteLn(' ', R);
    SPDX-License-Identifier: LGPL-3.0-or-later
    WriteLn(' ', R);
*)
Program RealValues;
Var
    R: Real;
Begin
    R := 123.;       WriteLn('123.       ', R);
    R := 123.45;     WriteLn('123.45     ', R);
    R := 1e2;        WriteLn('1e2        ', R);
    R := 2e+2;       WriteLn('2e+2       ', R);
    R := 3e-2;       WriteLn('3e-2       ', R);
    R := 1.2E34;     WriteLn('1.2E34     ', R);
    R := 1.2E+34;    WriteLn('1.2E+34    ', R);
    R := 1.2E-34;    WriteLn('1.2E-34    ', R);
    R := 123.45E-34; WriteLn('123.45E-34 ', R);
    R := 123.45E+34; WriteLn('123.45E+34 ', R);
    R := 123.456E34; WriteLn('123.456E34 ', R);
    // These one fail:
    // R := 1.2e-;      WriteLn('1.2e-      ', R);
    // R := 1.2e--34;   WriteLn('1.2e--34   ', R);
    // R := 1.2e+-34;   WriteLn('1.2e+-34   ', R);
    // R := 123..45E34; WriteLn('123..45E34 ', R);
End.
