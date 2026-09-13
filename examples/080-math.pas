(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Math; { With UTF-8 chars for fun! }

Var
    R: Real;

Begin
    WriteLn('Math constants and functions'                                                    );
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('01: ε         = ', EpsReal                                                  {:20:10});
    WriteLn('02: Min       = ', MinReal                                                  {:20:10});
    WriteLn('03: Max       = ', MaxReal                                                  {:20:10});
    WriteLn('04: 1.2E+34   = ', 1.23456789E+34                                           {:20:10});
    WriteLn('05: 1.2E-34   = ', 1.23456789e-34                                           {:20:10});
    WriteLn('06: |-1/3|    = ', Abs(-1/3)                                                :20:10);
    WriteLn('07: 1.0/3.0   = ', 1.0/3.0                                                  :20:10);
    WriteLn('08: π         = ', Pi                                                       :20:10);
    WriteLn('09: trunc(π)  = ', Trunc(Pi)                                                :20:10);
    WriteLn('10: frac(π)   = ', Frac(Pi)                                                 :20:10);
    WriteLn('11: round(π)  = ', Round(Pi)                                                :20:10);
    WriteLn('12: sin(π/4)  = ', Sin(Pi / 4.0)                                            :20:10);
    WriteLn('13: cos(π/4)  = ', Cos(Pi / 4.0)                                            :20:10);
    WriteLn('14: tan(π/4)  = ', Tan(Pi / 4.0)                                            :20:10);
    WriteLn('15: atn(1)    = ', ArcTan(1.0)                                              :20:10);
    WriteLn('16: 999²      = ', Sqr(999.0)                                               :20:10);
    WriteLn('17: √2        = ', Sqrt(2.0)                                                :20:10);
    WriteLn('18: √2/2      = ', Sqrt(2.0) / 2.0                                          :20:10);
    WriteLn('19: ℇ         = ', Exp(1.0)                                                 :20:10);
    WriteLn('20: round(ℇ)  = ', Round(Exp(1.0))                                          :20:10);
    WriteLn('21: trunc(ℇ)  = ', Trunc(Exp(1.0))                                          :20:10);
    WriteLn('22: frac(ℇ)   = ', Frac(Exp(1.0))                                           :20:10);
    WriteLn('23: ln(3)     = ', Ln(3.0)                                                  :20:10);
    WriteLn('24: log10(3)  = ', Log10(3.0)                                               :20:10);
    WriteLn('25: log2(3)   = ', Log2(3.0)                                                :20:10);
    WriteLn('26: logn(10,3)= ', LogN(10.0, 3.0)                                          :20:10);
    WriteLn('27: logn(2,3) = ', LogN(2.0, 3.0)                                           :20:10);
    WriteLn('28: 1.2^3.4   = ', Power(1.2, 3.4)                                          :20:10);
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('OK!'                                                                             );
End.
