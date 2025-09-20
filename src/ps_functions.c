/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <math.h>
#include <string.h>
#include <time.h>

#include "ps_error.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_string.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_value.h"

ps_error ps_function_exec(ps_interpreter *interpreter, ps_symbol *symbol, ps_value *value, ps_value *result)
{
    ps_function_1arg function = symbol->value->data.x->system;
    if (function == NULL)
        return PS_ERROR_NOT_IMPLEMENTED;
    return function(interpreter, value, result);
}

/******************************************************************************/
/* ORDINAL                                                                    */
/******************************************************************************/

/** @brief ODD - true if integer/unsigned value is odd, false if even */
ps_error ps_function_odd(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
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
/* "MATH"                                                                     */
/******************************************************************************/

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

ps_error ps_function_random(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    if (value == NULL)
    {
        // no argument, return random real between 0.0 and 1.0 excluded
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

/** @brief ABS(INTEGER|UNSIGNED|REAL): INTEGER|UNSIGNED|REAL - Get absolute value of integer / unsigned / real */
ps_error ps_function_abs(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
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

/** @brief GETTICKCOUNT(): UNSIGNED - Get milliseconds since program start */
ps_error ps_function_get_tick_count(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    // NB: value parameter is not used
    result->type = &ps_system_unsigned;
    clock_t c = clock();
    result->data.u = (ps_unsigned)((c * 1000) / CLOCKS_PER_SEC);
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
    double r;
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        result->type = &ps_system_real;
        modf(value->data.r, &r);
        result->data.r = r;
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief FRAC(REAL): REAL - Get fractional part of floating point value */
ps_error ps_function_frac(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    double int_part;
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        result->type = &ps_system_real;
        result->data.r = modf(value->data.r, &int_part);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief SIN(REAL): REAL - Get sinus of floating point value */
ps_error ps_function_sin(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        result->type = &ps_system_real;
        result->data.r = sin(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief COS(REAL): REAL - Get cosinus of floating point value */
ps_error ps_function_cos(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        result->type = &ps_system_real;
        result->data.r = cos(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief TAN(REAL): REAL - Get tangent of floating point value */
ps_error ps_function_tan(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    double c;
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        c = cos(value->data.r);
        if (c == 0.0)
            return PS_ERROR_DIVISION_BY_ZERO;
        result->type = &ps_system_real;
        result->data.r = sin(value->data.r) / c;
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief ARCTAN(REAL): REAL - Get arc tangent of floating point value */
ps_error ps_function_arctan(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        result->type = &ps_system_real;
        result->data.r = atan(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief SQR(REAL): REAL - Get square of floating point value */
ps_error ps_function_sqr(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        result->type = &ps_system_real;
        result->data.r = value->data.r * value->data.r;
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief SQRT(REAL): REAL - Get square root of floating point value */
ps_error ps_function_sqrt(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        if (value->data.r < 0.0)
            return PS_ERROR_OUT_OF_RANGE;
        result->type = &ps_system_real;
        result->data.r = sqrt(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief EXP(REAL): REAL - Get exponential of floating point value */
ps_error ps_function_exp(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        result->type = &ps_system_real;
        result->data.r = exp(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief LN(REAL): REAL - Get logarithm of floating point value */
ps_error ps_function_ln(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        if (value->data.r <= 0.0)
            return PS_ERROR_OUT_OF_RANGE;
        result->type = &ps_system_real;
        result->data.r = log(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}

/** @brief LOG(REAL): REAL - Get base 10 logarithm of floating point value */
ps_error ps_function_log(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_REAL:
        if (value->data.r <= 0.0)
            return PS_ERROR_OUT_OF_RANGE;
        result->type = &ps_system_real;
        result->data.r = log10(value->data.r);
        break;
    default:
        return PS_ERROR_EXPECTED_REAL;
    }
    return PS_ERROR_NONE;
}
