/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_FUNCTIONS_H
#define _PS_FUNCTIONS_H

#include <stdio.h>

#include "ps_ast.h"
#include "ps_interpreter.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    // Math
    extern ps_symbol ps_system_function_abs;
    extern ps_symbol ps_system_function_arctan;
    extern ps_symbol ps_system_function_cos;
    extern ps_symbol ps_system_function_even;
    extern ps_symbol ps_system_function_exp;
    extern ps_symbol ps_system_function_frac;
    extern ps_symbol ps_system_function_int;
    extern ps_symbol ps_system_function_ln;
    extern ps_symbol ps_system_function_log;
    extern ps_symbol ps_system_function_odd;
    extern ps_symbol ps_system_function_power;
    extern ps_symbol ps_system_function_random;
    extern ps_symbol ps_system_function_round;
    extern ps_symbol ps_system_function_sin;
    extern ps_symbol ps_system_function_sqr;
    extern ps_symbol ps_system_function_sqrt;
    extern ps_symbol ps_system_function_tan;
    extern ps_symbol ps_system_function_trunc;

    // Ordinal
    extern ps_symbol ps_system_function_chr;
    extern ps_symbol ps_system_function_high;
    extern ps_symbol ps_system_function_low;
    extern ps_symbol ps_system_function_ord;
    extern ps_symbol ps_system_function_pred;
    extern ps_symbol ps_system_function_succ;

    // String
    extern ps_symbol ps_system_function_length;
    extern ps_symbol ps_system_function_lowercase;
    extern ps_symbol ps_system_function_uppercase;

    // System
    extern ps_symbol ps_system_function_get_tick_count;

    /** @brief Add base functions to system */
    bool ps_functions_init(ps_ast_block *system);

    /** @brief Execute a function with 1 argument */
    ps_error ps_function_exec_1arg(ps_interpreter *interpreter, const ps_symbol *symbol, const ps_value *value,
                                   ps_value *result);

    /** @brief Execute a function with 1 symbolic argument */
    ps_error ps_function_exec_1arg_s(ps_interpreter *interpreter, const ps_symbol *symbol, ps_symbol *type,
                                     ps_value *result);

    /** @brief Execute a function with 2 arguments */
    ps_error ps_function_exec_2args(ps_interpreter *interpreter, const ps_symbol *symbol, const ps_value *a,
                                    const ps_value *b, ps_value *result);

    /** @brief ABS(INTEGER|UNSIGNED|REAL): INTEGER|UNSIGNED|REAL - Get absolute value of integer, unsigned or real */
    ps_error ps_function_abs(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief ARCTAN(REAL): REAL - Get arc tangent of floating point value */
    ps_error ps_function_arctan(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief COS(REAL): REAL - Get cosinus of floating point value */
    ps_error ps_function_cos(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief EVEN - true if integer/unsigned value is even, false if odd */
    ps_error ps_function_even(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief EXP(REAL): REAL - Get exponential of floating point value */
    ps_error ps_function_exp(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief FRAC(REAL): REAL - Get fractional part of floating point value */
    ps_error ps_function_frac(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief INT(REAL): REAL - Get integer part of floating point value */
    ps_error ps_function_int(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief LN(REAL): REAL - Get logarithm of floating point value */
    ps_error ps_function_ln(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief LOG(REAL): REAL - Get base 10 logarithm of floating point value */
    ps_error ps_function_log(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief ODD - true if integer/unsigned value is odd, false if even */
    ps_error ps_function_odd(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief RANDOM([INTEGER|UNSIGNED]): REAL|INTEGER|UNSIGNED - Get random value,
     *         either real between 0 and 1 (excluded) or between 0 and N - 1
     */
    ps_error ps_function_random(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief ROUND(REAL): INTEGER - Round real as integer */
    ps_error ps_function_round(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief SIN(REAL): REAL - Get sinus of floating point value */
    ps_error ps_function_sin(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief SQR(REAL): REAL - Get square (x²) of floating point value */
    ps_error ps_function_sqr(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief SQRT(REAL): REAL - Get square root (√x) of floating point value */
    ps_error ps_function_sqrt(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief TAN(REAL): REAL - Get tangent of floating point value */
    ps_error ps_function_tan(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief TRUNC(REAL): INTEGER - Truncate real as integer */
    ps_error ps_function_trunc(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief POWER(REAL, REAL): REAL - Get power of floating point value */
    ps_error ps_function_power(ps_interpreter *interpreter, const ps_value *a, const ps_value *b, ps_value *result);

    /** @brief CHR - Get char value of unsigned / integer or subrange value */
    ps_error ps_function_chr(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief HIGH - Get highest value of ordinal type */
    ps_error ps_function_high(ps_interpreter *interpreter, ps_symbol *type, ps_value *result);

    /** @brief LOW - Get lowest value of ordinal type */
    ps_error ps_function_low(ps_interpreter *interpreter, ps_symbol *type, ps_value *result);

    /** @brief ORD - Get ordinal value of boolean / char / enum */
    ps_error ps_function_ord(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief PRED - Get previous value (predecessor) of scalar or ordinal value */
    ps_error ps_function_pred(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief SUCC - Get next value (successor) of ordinal value */
    ps_error ps_function_succ(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief LENGTH(): UNSIGNED - Get string length */
    ps_error ps_function_length(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief LOWERCASE(STRING): STRING - Compute lowercase string */
    ps_error ps_function_lowercase(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief UPPERCASE(STRING): STRING - Compute uppercase string */
    ps_error ps_function_uppercase(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

    /** @brief GETTICKCOUNT(): UNSIGNED - Get milliseconds since program start */
    ps_error ps_function_get_tick_count(ps_interpreter *interpreter, const ps_value *value, ps_value *result);

#ifdef __cplusplus
}
#endif

#endif /* _PS_FUNCTIONS_H */
