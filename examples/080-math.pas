(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program Math; { With UTF-8 chars for fun! }

Var
  I: Integer;

Begin
    WriteLn('Math constants and functions'                                                    );
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('01 ε        = ', EpsReal                                                            );
    WriteLn('02 Min      = ', MinReal                                                            );
    WriteLn('03 Max      = ', MaxReal                                                            );
    WriteLn('04 1.2E+34  = ', 1.23456789E+34                                                     );
    WriteLn('05 1.2E-34  = ', 1.23456789e-34                                                     );
    WriteLn('06 |-1/3|   = ', Abs(-1/3)                                                          );
    WriteLn('07 1.0/3.0  = ', 1.0/3.0                                                            );
    WriteLn('08 π        = ', Pi                                                                 );
    WriteLn('09 trunc(π) = ', Trunc(Pi)                                                          );
    WriteLn('10 frac(π)  = ', Frac(Pi)                                                           );
    WriteLn('11 round(π) = ', Round(Pi)                                                          );
    WriteLn('12 sin(π/4) = ', Sin(Pi / 4.0)                                                      );
    WriteLn('13 cos(π/4) = ', Cos(Pi / 4.0)                                                      );
    WriteLn('14 tan(π/4) = ', Tan(Pi / 4.0)                                                      );
    WriteLn('15 atn(1)   = ', ArcTan(1.0)                                                        );
    WriteLn('16 999²     = ', Sqr(999.0)                                                         );
    WriteLn('17 √2       = ', Sqrt(2.0)                                                          );
    WriteLn('18 √2/2     = ', Sqrt(2.0) / 2.0                                                    );
    WriteLn('19 e        = ', Exp(1.0)                                                           );
    WriteLn('20 round(e) = ', Round(Exp(1.0))                                                    );
    WriteLn('21 1.2^3.4  = ', Power(1.2, 3.4)                                                    );
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('OK!'                                                                             );
End.
