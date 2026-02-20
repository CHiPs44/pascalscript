(*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*)
Program TestOperators;

Var
    i, j, k: Integer;
    u, v, w: Unsigned;
    x, y, z: Real;
    c1, c2: Char;
    s1, s2, s3: String;
    b1, b2, b3: Boolean;

Begin
    WriteLn('TODO addition below should display $FFFFFFFF in decimal, but it displays -1 because of the signed/unsigned mismatch somewhere...');
    WriteLn(MaxInt, ' ', $55555555, '+', $AAAAAAAA, '=', $55555555 + $AAAAAAAA, ' ', $FFFFFFFF, ' ', MaxUInt);
    WriteLn();

    WriteLn('PascalScript Operators Demo');
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn();
    WriteLn('* Assignment operator');
    k := 42;
    WriteLn('  - Integer:                        = ', k:12);
    u := $FFFFFFFF;
    WriteLn('  - Unsigned:                       = ', u:12);
    z := 3.14;
    WriteLn('  - Real:                           = ', z:12:8);
    c1 := 'A';
    WriteLn('  - Char:                           = ''', c1, '''');
    s1 := 'PascalScript';
    WriteLn('  - String:                         = ''', s1, '''');
    b3 := True;
    WriteLn('  - Boolean:                        = ', b3);

    WriteLn('* Arithmetic Integer operators');
    i := 10;
    j := 3;
    k := i + j;
    WriteLn('  - Addition:              10  +  3 = ', k:12);
    k := i - j;
    WriteLn('  - Subtraction:           10  -  3 = ', k:12);
    k := i * j;
    WriteLn('  - Multiplication:        10  *  3 = ', k:12);
    k := i Div j;
    WriteLn('  - Division:              10 Div 3 = ', k:12);
    k := i Mod j;
    WriteLn('  - Modulo:                10 Mod 3 = ', k:12);

    WriteLn('* Arithmetic Unsigned operators');
    u := 10;
    v := 3;
    w := u + v;
    WriteLn('  - Addition:              10  +  3 = ', w:12);
    w := u - v;
    WriteLn('  - Subtraction:           10  -  3 = ', w:12);
    w := u * v;
    WriteLn('  - Multiplication:        10  *  3 = ', w:12);
    w := u Div v;
    WriteLn('  - Division:              10 Div 3 = ', w:12);
    w := u Mod v;
    WriteLn('  - Modulo:                10 Mod 3 = ', w:12);

    WriteLn('* Arithmetic Real operators');
    x := 10.0;
    y := 3.0;
    z := x + y;
    WriteLn('  - Addition:              10.0 + 3.0 = ', z:12:8);
    z := x - y;
    WriteLn('  - Subtraction:           10.0 - 3.0 = ', z:12:8);
    z := x * y;
    WriteLn('  - Multiplication:        10.0 * 3.0 = ', z:12:8);
    z := x / y;
    WriteLn('  - Division:              10.0 / 3.0 = ', z:12:8);

    WriteLn('* Unary operators');
    k := MaxInt;
    k := -k;
    WriteLn('  - Unary minus:           -Maxint       = ', k:12);
    b3 := Not (i > j);
    WriteLn('  - Logical not:           Not (10 > 3) is ', b3);
    v := $55555555;
    u := Not v;
    WriteLn('  - Bitwise not:           Not $55555555 = ', u);
    If u = $AAAAAAAA then
        WriteLn('    (matches expected value $AAAAAAAA)')
    else
        WriteLn('    (does NOT match expected value $AAAAAAAA)');

    WriteLn('* Comparison operators');
    b1 := i = j;
    WriteLn('  - Equal:                 10 =  3 is ', b1);
    b1 := i <> j;
    WriteLn('  - Not equal:             10 <> 3 is ', b1);
    b1 := i < j;
    WriteLn('  - Less than:             10 <  3 is ', b1);
    b1 := i > j;
    WriteLn('  - Greater than:          10 >  3 is ', b1);
    b1 := i <= j;
    WriteLn('  - Less or equal:         10 <= 3 is ', b1);
    b1 := i >= j;
    WriteLn('  - Greater or equal:      10 >= 3 is ', b1);

    WriteLn('* Logical operators');
    b1 := True;
    b2 := False;
    b3 := b1 And b2;
    WriteLn('  - And:                   True And False is ', b3);
    b3 := b1 Or b2;
    WriteLn('  - Or:                    True Or  False is ', b3);
    b3 := Not b1;
    WriteLn('  - Not:                   Not True       is ', b3);
    b3 := Not b2;
    WriteLn('  - Not:                   Not False      is ', b3);
    b3 := b1 Xor b2;
    WriteLn('  - Xor:                   True Xor False is ', b3);

    WriteLn('* String operators');
    s1 := 'Hello';
    s2 := 'World';
    s3 := s1 + ' ' + s2;
    WriteLn('  - String + String:       ''', s1, ''' + ''', s2, ''' = ''', s3, '''');
    c1 := 'H';
    c2 := 'W';
    s3 := c1 + ' ' + c2;
    WriteLn('  - Char + Char:           ''', c1, '''     + ''', c2, '''     = ''', s3, '''');
    s1 := 'Hello';
    c2 := 'W';
    s3 := s1 + ' ' + c2;
    WriteLn('  - String + Char:         ''', s1, ''' + ''', c2, '''     = ''', s3, '''');
    c1 := 'H';
    s2 := 'World';
    s3 := c1 + ' ' + s2;
    WriteLn('  - Char + String:         ''', c1, '''     + ''', s2, ''' = ''', s3, '''');

    WriteLn('* Type mixing and real promotion');
    u := 5;
    i := 2;
    x := 3.5;
    y := 3.0;
    z := i + x;
    WriteLn('  - Integer + Real:        2 + 3.5     = ', z:12:8);
    z := u * x;
    WriteLn('  - Unsigned * Real:       5 * 3.5     = ', z:12:8);
    z := i / u;
    WriteLn('  - Integer / Unsigned:    2 / 5       = ', z:12:8);
    z := u + i + x;
    WriteLn('  - Mixed:                 5 + 2 + 3.5 = ', z:12:8);

    WriteLn('* Complex expression');
    i := 10;
    j := 3;
    k := 42;
    u := 5;
    x := 3.5;
    z := (i + j) * x / u - k; // Mixed expression with parentheses
    WriteLn('  - Complex expression:    (10 + 3) * 3.5 / 5 - 42 = ', z:12:8);

    WriteLn();
    WriteLn('--------------------------------------------------------------------------------');
    WriteLn('OK');
END.
