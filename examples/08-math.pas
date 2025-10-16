(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Example08Math;
//Uses FreePascalCompatibility32;
{ With UTF-8 chars for fun! }
Begin
  WriteLn('PascalScript version ', PS_VERSION, ' (', PS_BITNESS, ' bits)');
  WriteLn;
  WriteLn('===================================');
  WriteLn('| Math constants and functions    |');
  WriteLn('===================================');
  WriteLn('ε        = ', EpsReal);
  WriteLn('Min      = ', MinReal);
  WriteLn('Max      = ', MaxReal);
  WriteLn('1.2E+34  = ', 1.23456789E+34);
  WriteLn('1.2E-34  = ', 1.23456789e-34);
  WriteLn('1/3      = ', Abs(-1/3));
  WriteLn('π        = ', Pi);
  WriteLn('trunc(π) = ', Trunc(Pi));
  WriteLn('frac(π)  = ', Frac(Pi));
  WriteLn('round(π) = ', Round(Pi));
  WriteLn('sin(π/4) = ', Sin(Pi / 4.0));
  WriteLn('cos(π/4) = ', Cos(Pi / 4.0));
  WriteLn('tan(π/4) = ', Tan(Pi / 4.0));
  WriteLn('atn(1)   = ', ArcTan(1.0));
  WriteLn('999²     = ', Sqr(999.0));
  WriteLn('√2/2     = ', Sqrt(2.0) / 2.0);
  WriteLn('e        = ', Exp(1.0));
  WriteLn('round(e) = ', Round(Exp(1.0)));
  WriteLn('OK!');
End.
