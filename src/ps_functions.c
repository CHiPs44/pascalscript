/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <string.h>
#include <time.h>

#include "ps_environment.h"
#include "ps_error.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_string.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_value.h"

/******************************************************************************/
/* STANDARD FUNCTIONS                                                         */
/******************************************************************************/

/* clang-format off */
PS_SYSTEM_FUNCTION (function , abs           , "ABS"         , .func_1arg      , &ps_function_abs               );
PS_SYSTEM_FUNCTION (function , arctan        , "ARCTAN"      , .func_1arg      , &ps_function_arctan            );
PS_SYSTEM_FUNCTION (function , chr           , "CHR"         , .func_1arg      , &ps_function_chr               );
PS_SYSTEM_FUNCTION (function , cos           , "COS"         , .func_1arg      , &ps_function_cos               );
PS_SYSTEM_FUNCTION (function , even          , "EVEN"        , .func_1arg      , &ps_function_even              );
PS_SYSTEM_FUNCTION (function , exp           , "EXP"         , .func_1arg      , &ps_function_exp               );
PS_SYSTEM_FUNCTION (function , frac          , "FRAC"        , .func_1arg      , &ps_function_frac              );
PS_SYSTEM_FUNCTION (function , get_tick_count, "GETTICKCOUNT", .func_1arg      , &ps_function_get_tick_count    );
PS_SYSTEM_FUNCTION (function , int           , "INT"         , .func_1arg      , &ps_function_int               );
PS_SYSTEM_FUNCTION (function , length        , "LENGTH"      , .func_1arg      , &ps_function_length            );
PS_SYSTEM_FUNCTION (function , ln            , "LN"          , .func_1arg      , &ps_function_ln                );
PS_SYSTEM_FUNCTION (function , log           , "LOG"         , .func_1arg      , &ps_function_log               );
PS_SYSTEM_FUNCTION (function , lowercase     , "LOWERCASE"   , .func_1arg      , &ps_function_lowercase         );
PS_SYSTEM_FUNCTION (function , odd           , "ODD"         , .func_1arg      , &ps_function_odd               );
PS_SYSTEM_FUNCTION (function , ord           , "ORD"         , .func_1arg      , &ps_function_ord               );
PS_SYSTEM_FUNCTION (function , power         , "POWER"       , .func_2args     , &ps_function_power             );
PS_SYSTEM_FUNCTION (function , pred          , "PRED"        , .func_1arg      , &ps_function_pred              );
PS_SYSTEM_FUNCTION (function , random        , "RANDOM"      , .func_1arg      , &ps_function_random            );
PS_SYSTEM_FUNCTION (function , round         , "ROUND"       , .func_1arg      , &ps_function_round             );
PS_SYSTEM_FUNCTION (function , sin           , "SIN"         , .func_1arg      , &ps_function_sin               );
PS_SYSTEM_FUNCTION (function , sqr           , "SQR"         , .func_1arg      , &ps_function_sqr               );   
PS_SYSTEM_FUNCTION (function , sqrt          , "SQRT"        , .func_1arg      , &ps_function_sqrt              );
PS_SYSTEM_FUNCTION (function , succ          , "SUCC"        , .func_1arg      , &ps_function_succ              );
PS_SYSTEM_FUNCTION (function , tan           , "TAN"         , .func_1arg      , &ps_function_tan               );
PS_SYSTEM_FUNCTION (function , trunc         , "TRUNC"       , .func_1arg      , &ps_function_trunc             );
PS_SYSTEM_FUNCTION (function , uppercase     , "UPPERCASE"   , .func_1arg      , &ps_function_uppercase         );
/* clang-format on */

bool ps_functions_init(ps_environment *system)
{
    ADD_SYSTEM_SYMBOL(ps_system_function_abs);
    ADD_SYSTEM_SYMBOL(ps_system_function_arctan);
    ADD_SYSTEM_SYMBOL(ps_system_function_chr);
    ADD_SYSTEM_SYMBOL(ps_system_function_cos);
    ADD_SYSTEM_SYMBOL(ps_system_function_even);
    ADD_SYSTEM_SYMBOL(ps_system_function_exp);
    ADD_SYSTEM_SYMBOL(ps_system_function_frac);
    ADD_SYSTEM_SYMBOL(ps_system_function_get_tick_count);
    ADD_SYSTEM_SYMBOL(ps_system_function_int);
    ADD_SYSTEM_SYMBOL(ps_system_function_length);
    ADD_SYSTEM_SYMBOL(ps_system_function_ln);
    ADD_SYSTEM_SYMBOL(ps_system_function_log);
    ADD_SYSTEM_SYMBOL(ps_system_function_lowercase);
    ADD_SYSTEM_SYMBOL(ps_system_function_odd);
    ADD_SYSTEM_SYMBOL(ps_system_function_ord);
    ADD_SYSTEM_SYMBOL(ps_system_function_power);
    ADD_SYSTEM_SYMBOL(ps_system_function_pred);
    ADD_SYSTEM_SYMBOL(ps_system_function_random);
    ADD_SYSTEM_SYMBOL(ps_system_function_round);
    ADD_SYSTEM_SYMBOL(ps_system_function_sin);
    ADD_SYSTEM_SYMBOL(ps_system_function_sqr);
    ADD_SYSTEM_SYMBOL(ps_system_function_sqrt);
    ADD_SYSTEM_SYMBOL(ps_system_function_succ);
    ADD_SYSTEM_SYMBOL(ps_system_function_tan);
    ADD_SYSTEM_SYMBOL(ps_system_function_trunc);
    ADD_SYSTEM_SYMBOL(ps_system_function_uppercase);
    return true;
error:
    return false;
}

ps_error ps_function_exec_1arg(ps_interpreter *interpreter, ps_symbol *symbol, ps_value *value, ps_value *result)
{
    assert(interpreter != NULL);
    assert(symbol != NULL);
    assert(symbol->value != NULL);
    assert(symbol->value->data.x != NULL);
    assert(result != NULL);
    ps_function_1arg function = (ps_function_1arg)(symbol->value->data.x->func_1arg);
    if (function == NULL)
    {
        ps_interpreter_set_message(interpreter, "Function '%s' not implemented", symbol->name);
        return PS_ERROR_NOT_IMPLEMENTED;
    }
    return function(interpreter, value, result);
}

ps_error ps_function_exec_2args(ps_interpreter *interpreter, ps_symbol *symbol, ps_value *a, ps_value *b,
                                ps_value *result)
{
    assert(interpreter != NULL);
    assert(symbol != NULL);
    assert(symbol->value != NULL);
    assert(symbol->value->data.x != NULL);
    assert(a != NULL);
    assert(b != NULL);
    assert(result != NULL);
    ps_function_2args function = (ps_function_2args)(symbol->value->data.x->func_2args);
    if (function == NULL)
    {
        ps_interpreter_set_message(interpreter, "Function '%s' not implemented", symbol->name);
        return PS_ERROR_NOT_IMPLEMENTED;
    }
    return function(interpreter, a, b, result);
}

/******************************************************************************/
/* ORDINAL                                                                    */
/******************************************************************************/

/** @brief ODD - true if integer/unsigned value is odd, false if even */
ps_error ps_function_odd(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    result->type = &ps_system_boolean;
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_UNSIGNED:
        result->data.b = (ps_boolean)((value->data.u & 1) != 0);
        break;
    case PS_TYPE_INTEGER:
        result->data.b = (ps_boolean)((value->data.i & 1) != 0);
        break;
    default:
        return PS_ERROR_UNEXPECTED_TYPE;
    }
    return PS_ERROR_NONE;
}

/** @brief EVEN - true if integer/unsigned value is even, false if odd */
ps_error ps_function_even(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    result->type = &ps_system_boolean;
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_UNSIGNED:
        result->data.b = (ps_boolean)((value->data.u & 1) == 0);
        break;
    case PS_TYPE_INTEGER:
        result->data.b = (ps_boolean)((value->data.i & 1) == 0);
        break;
    default:
        return PS_ERROR_UNEXPECTED_TYPE;
    }
    return PS_ERROR_NONE;
}

/** @brief ORD - Get ordinal value of boolean / char */
ps_error ps_function_ord(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_UNSIGNED:
        // case PS_TYPE_ENUM:
        result->type = &ps_system_unsigned;
        result->data.u = value->data.u;
        break;
    case PS_TYPE_INTEGER:
        // case PS_TYPE_SUBRANGE:
        result->type = &ps_system_integer;
        result->data.i = value->data.i;
        break;
    case PS_TYPE_BOOLEAN:
        // ord(false) => 0 / ord(true) => 1
        result->type = &ps_system_integer;
        result->data.i = value->data.b ? 1 : 0;
        break;
    case PS_TYPE_CHAR:
        // ord('0') => 48 / ord('A') => 65 / ...
        result->type = &ps_system_unsigned;
        result->data.u = (ps_unsigned)(value->data.c);
        break;
    default:
        return PS_ERROR_UNEXPECTED_TYPE;
    }
    return PS_ERROR_NONE;
}

/** @brief CHR - Get char value of unsigned / integer or subrange value */
ps_error ps_function_chr(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_UNSIGNED:
        if (interpreter->range_check && value->data.u > PS_CHAR_MAX)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.c = (ps_char)(value->data.u);
        break;
    case PS_TYPE_INTEGER:
        if (interpreter->range_check && (value->data.i < 0 || value->data.i > PS_CHAR_MAX))
            return PS_ERROR_OUT_OF_RANGE;
        result->data.c = (ps_char)(value->data.i);
        break;
    default:
        return PS_ERROR_UNEXPECTED_TYPE;
    }
    result->type = &ps_system_char;
    return PS_ERROR_NONE;
}

/** @brief PRED - Get previous value (predecessor) of scalar value */
ps_error ps_function_pred(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_INTEGER:
        // pred(min) => error / pred(i) => i - 1
        if (interpreter->range_check && value->data.i == PS_INTEGER_MIN)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.i = value->data.i - 1;
        break;
    // case PS_TYPE_SUBRANGE:
    //   TODO needs low()
    case PS_TYPE_UNSIGNED:
        // pred(0) => error / pred(u) => u - 1
        if (interpreter->range_check && value->data.u == 0)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.u = value->data.u - 1;
        break;
    // case PS_TYPE_ENUM:
    //   TODO needs low()
    case PS_TYPE_BOOLEAN:
        // pred(true) => false / pred(false) => error
        if (interpreter->range_check && value->data.b == false)
            return PS_ERROR_OUT_OF_RANGE;
        // this will make pred(false) = false
        result->data.b = (ps_boolean) false;
        break;
    case PS_TYPE_CHAR:
        // pred(NUL) => error / pred(c) => c - 1
        if (interpreter->range_check && value->data.c == 0)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.c = value->data.c - 1;
        break;
    default:
        return PS_ERROR_UNEXPECTED_TYPE;
    }
    result->type = value->type;
    return PS_ERROR_NONE;
}

/** @brief SUCC - Get next value (successor) of ordinal value */
ps_error ps_function_succ(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_INTEGER:
        // succ(max) => error / succ(i) => i + 1
        if (interpreter->range_check && value->data.u == PS_INTEGER_MAX)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.i = value->data.i + 1;
        break;
    // case PS_TYPE_SUBRANGE:
    //   TODO needs high()
    case PS_TYPE_UNSIGNED:
        // succ(max) => error / succ(u) => u + 1
        if (interpreter->range_check && value->data.u == PS_UNSIGNED_MAX)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.u = value->data.u + 1;
        break;
    // case PS_TYPE_ENUM:
    //   TODO needs high()
    case PS_TYPE_BOOLEAN:
        // succ(true) => error / succ(false) => true
        if (interpreter->range_check && value->data.b == true)
            return PS_ERROR_OUT_OF_RANGE;
        // this will make succ(true) = true
        result->data.b = true;
        break;
    case PS_TYPE_CHAR:
        // succ(char_max) => error / succ(c) => c + 1
        if (interpreter->range_check && value->data.c == PS_CHAR_MAX)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.c = value->data.c + 1;
        break;
    default:
        return PS_ERROR_UNEXPECTED_TYPE;
    }
    result->type = value->type;
    return PS_ERROR_NONE;
}

/******************************************************************************/
/* MATH                                                                       */
/******************************************************************************/

/** @brief ABS(INTEGER|UNSIGNED|REAL): INTEGER|UNSIGNED|REAL - Get absolute value of integer, unsigned or real */
ps_error ps_function_abs(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    switch (value->type->value->data.t->type)
    {
    case PS_TYPE_UNSIGNED:
        // abs(u) => u
        result->data.u = value->data.u;
        break;
    case PS_TYPE_INTEGER:
        result->data.i = (ps_integer)abs(value->data.i);
        break;
    case PS_TYPE_REAL:
        result->data.r = (ps_real)fabs(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_NUMBER;
    }
    result->type = value->type;
    return PS_ERROR_NONE;
}

/** @brief TRUNC(REAL): INTEGER - Truncate real as integer */
ps_error ps_function_trunc(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->value->data.t->type)
    {
    case PS_TYPE_REAL:
        if (interpreter->range_check && (value->data.r < PS_INTEGER_MIN || value->data.r > PS_INTEGER_MAX))
            return PS_ERROR_OUT_OF_RANGE;
        result->type = &ps_system_integer;
        result->data.i = (ps_integer)trunc(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief ROUND(REAL): INTEGER - Round real as integer */
ps_error ps_function_round(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    double r;
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        r = round(value->data.r);
        if (interpreter->range_check && (r < PS_INTEGER_MIN || r > PS_INTEGER_MAX))
            return PS_ERROR_OUT_OF_RANGE;
        result->type = &ps_system_integer;
        result->data.i = (ps_integer)r;
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief INT(REAL): REAL - Get integer part of floating point value */
ps_error ps_function_int(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    double r;
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        result->type = &ps_system_real;
        modf(value->data.r, &r);
        result->data.r = (ps_real)r;
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief FRAC(REAL): REAL - Get fractional part of floating point value */
ps_error ps_function_frac(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    double int_part;
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        result->type = &ps_system_real;
        result->data.r = (ps_real)modf(value->data.r, &int_part);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief SIN(REAL): REAL - Get sinus of floating point value */
ps_error ps_function_sin(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        result->type = &ps_system_real;
        result->data.r = (ps_real)sin(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief COS(REAL): REAL - Get cosinus of floating point value */
ps_error ps_function_cos(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        result->data.r = (ps_real)cos(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    result->type = &ps_system_real;
    return PS_ERROR_NONE;
}

/** @brief TAN(REAL): REAL - Get tangent of floating point value */
ps_error ps_function_tan(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    double c, s;
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        c = cos(value->data.r);
        if (c == 0.0)
            return PS_ERROR_DIVISION_BY_ZERO;
        s = sin(value->data.r);
        result->data.r = (ps_real)(s / c);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    result->type = &ps_system_real;
    return PS_ERROR_NONE;
}

/** @brief ARCTAN(REAL): REAL - Get arc tangent of floating point value */
ps_error ps_function_arctan(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    double r;
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        r = atan(value->data.r);
        if (errno != 0 || isnan(r) || isinf(r))
            return PS_ERROR_MATH_NAN_INF;
        if (interpreter->range_check && (r < PS_REAL_MIN || r > PS_REAL_MAX))
            return PS_ERROR_OUT_OF_RANGE;
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    result->type = &ps_system_real;
    result->data.r = (ps_real)r;
    return PS_ERROR_NONE;
}

/** @brief SQR(REAL): REAL - Get square (x²) of floating point value */
ps_error ps_function_sqr(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    ps_real r;
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        // TODO range check for overflow?
        r = value->data.r * value->data.r;
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    result->type = &ps_system_real;
    result->data.r = r;
    return PS_ERROR_NONE;
}

/** @brief SQRT(REAL): REAL - Get square root (√x) of floating point value */
ps_error ps_function_sqrt(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        if (value->data.r < 0.0)
            return PS_ERROR_OUT_OF_RANGE;
        result->type = &ps_system_real;
        result->data.r = (ps_real)sqrt(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief EXP(REAL): REAL - Get exponential of floating point value */
ps_error ps_function_exp(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        result->type = &ps_system_real;
        // TODO range check for overflow?
        result->data.r = (ps_real)exp(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief LN(REAL): REAL - Get logarithm of floating point value */
ps_error ps_function_ln(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        if (value->data.r <= 0.0)
            return PS_ERROR_OUT_OF_RANGE;
        result->type = &ps_system_real;
        result->data.r = (ps_real)log(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief LOG(REAL): REAL - Get base 10 logarithm of floating point value */
ps_error ps_function_log(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        if (value->data.r <= 0.0)
            return PS_ERROR_OUT_OF_RANGE;
        result->type = &ps_system_real;
        result->data.r = (ps_real)log10(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief POWER(REAL, REAL): REAL - Get power of floating point value */
ps_error ps_function_power(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)
{
    ((void)interpreter);
    if (a->type->value->data.t->base != PS_TYPE_REAL || b->type->value->data.t->base != PS_TYPE_REAL)
        return PS_ERROR_EXPECTED_REAL;
    result->type = &ps_system_real;
    result->data.r = (ps_real)pow(a->data.r, b->data.r);
    return PS_ERROR_NONE;
}

/******************************************************************************/
/* STRINGS                                                                    */
/******************************************************************************/

/** @brief LENGTH(): UNSIGNED - Get string length */
ps_error ps_function_length(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    if (value->type->value->data.t->base != PS_TYPE_STRING)
        return PS_ERROR_EXPECTED_STRING;
    result->type = &ps_system_unsigned;
    result->data.u = value->data.s->len;
    return PS_ERROR_NONE;
}

ps_error ps_function_lowercase(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    if (value->type->value->data.t->base != PS_TYPE_STRING)
        return PS_ERROR_EXPECTED_STRING;
    result->type = &ps_system_string;
    result->data.s = ps_string_lowercase(value->data.s);
    if (result->data.s == NULL)
        return PS_ERROR_OUT_OF_MEMORY;
    return PS_ERROR_NONE;
}

ps_error ps_function_uppercase(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    if (value->type->value->data.t->base != PS_TYPE_STRING)
        return PS_ERROR_EXPECTED_STRING;
    result->type = &ps_system_string;
    result->data.s = ps_string_uppercase(value->data.s);
    if (result->data.s == NULL)
        return PS_ERROR_OUT_OF_MEMORY;
    return PS_ERROR_NONE;
}

/******************************************************************************/
/* OTHER                                                                      */
/******************************************************************************/

/** @brief GETTICKCOUNT(): UNSIGNED - Get milliseconds since program start */
ps_error ps_function_get_tick_count(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    // NB: value parameter is not used
    ((void)value);
    result->type = &ps_system_unsigned;
    clock_t c = clock();
    result->data.u = (ps_unsigned)((c * 1000) / CLOCKS_PER_SEC);
    return PS_ERROR_NONE;
}

// from https://www.cs.yale.edu/homes/aspnes/pinewiki/C(2f)Randomization.html
//   => correct "bias" introduced by modulus operator
int rand_range_integer(int n)
{
    int r;
    int limit;

    limit = RAND_MAX - (RAND_MAX % n);
    while ((r = rand()) >= limit)
        ;

    return r % n;
}

unsigned int rand_range_unsigned(unsigned int n)
{
    // use rand_range_integer() for small values
    if (n <= RAND_MAX)
    {
        return (unsigned int)rand_range_integer((int)n);
    }
    // use double for larger values
    return (unsigned int)((double)rand() * (double)n / (double)RAND_MAX);
}

/** @brief RANDOM([INTEGER|UNSIGNED]): REAL|INTEGER|UNSIGNED - Get random value,
 *         either real between 0 and 1 (excluded) or between 0 and N - 1
 */
ps_error ps_function_random(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    ((void)interpreter);
    if (value == NULL)
    {
        // no argument, return random real between 0.0 included and 1.0 excluded
        result->type = &ps_system_real;
        do
        {
            result->data.r = (double)rand() / (double)RAND_MAX;
        } while (result->data.r >= 1.0);
    }
    else
    {
        // one argument, return random integer / unsigned
        switch (value->type->value->data.t->base)
        {
        case PS_TYPE_INTEGER:
            result->type = &ps_system_integer;
            result->data.i = (ps_integer)rand_range_integer(value->data.i);
            break;
        case PS_TYPE_UNSIGNED:
            result->type = &ps_system_unsigned;
            result->data.u = (ps_unsigned)rand_range_unsigned(value->data.u);
            break;
        default:
            return PS_ERROR_UNEXPECTED_TYPE;
        }
    }
    return PS_ERROR_NONE;
}
