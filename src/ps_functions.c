/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <math.h>
#include <string.h>
#include <time.h>

#include "ps_error.h"
#include "ps_system.h"
#include "ps_value.h"
#include "ps_interpreter.h"
#include "ps_token.h"
#include "ps_functions.h"

bool ps_function_write_text(ps_interpreter *interpreter, FILE *f, ps_value *value)
{
    char *display_value = ps_value_get_display_value(value);
    if (display_value == NULL)
    {
        interpreter->error = PS_RUNTIME_ERROR_EXPECTED_STRING;
        return false;
    }
    if (interpreter->debug)
        fprintf(f, "WRITE\t'%s'\n", display_value);
    else
        fprintf(f, "%s", display_value);
    return true;
}

bool ps_function_randomize(ps_interpreter *interpreter)
{
    srand((unsigned int)time(NULL));
    return true;
}

bool ps_function_copy_value(ps_interpreter *interpreter, ps_value *from, ps_value *to)
{
    // ps_value_debug(stderr, "FROM\t", from);
    // ps_value_debug(stderr, "TO\t", to);
    if (to->type == NULL || to->type->base == PS_TYPE_NONE)
        to->type = from->type;
    if (from->type == to->type)
    {
        to->data = from->data;
        return true;
    }
    // Integer => Unsigned?
    if (from->type->base == PS_TYPE_INTEGER && to->type->base == PS_TYPE_UNSIGNED)
    {
        if (interpreter->range_check && from->data.i < 0)
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        to->data.u = from->data.i;
        return true;
    }
    // Unsigned => Integer?
    if (from->type->base == PS_TYPE_UNSIGNED && to->type->base == PS_TYPE_INTEGER)
    {
        if (interpreter->range_check && from->data.u > PS_INTEGER_MAX)
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        to->data.i = from->data.u;
        return true;
    }
    // Integer => Real?
    if (from->type->base == PS_TYPE_INTEGER && to->type->base == PS_TYPE_REAL)
    {
        to->data.r = (double)from->data.i;
        return true;
    }
    // Unsigned => Real?
    if (from->type->base == PS_TYPE_UNSIGNED && to->type->base == PS_TYPE_REAL)
    {
        to->data.r = (double)from->data.u;
        return true;
    }
    interpreter->error = PS_RUNTIME_ERROR_TYPE_MISMATCH;
    return false;
}

/******************************************************************************/
/* BASE                                                                       */
/******************************************************************************/

/** @brief Compute bitwise not for integer / unsigned, logical not for boolean, neg for integer / real */
bool ps_function_unary_op(ps_interpreter *interpreter, ps_value *value, ps_value *result, ps_token_type token_type)
{
    result->type = value->type;
    // NB: with FPC, not(subrange) or not(enum) yields integer result without range checking
    switch (value->type->base)
    {
    case PS_TYPE_INTEGER:
        /* clang-format off */
        switch (token_type)
        {
        case PS_TOKEN_NOT:      result->data.i = ~value->data.i; break;
        case PS_TOKEN_MINUS:    result->data.i = -value->data.i; break;
        default:                interpreter->error = PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE; return false;
        }
        /* clang-format on */
        break;
    case PS_TYPE_UNSIGNED:
        /* clang-format off */
        switch (token_type)
        {
        case PS_TOKEN_NOT:      result->data.u = ~value->data.u; break;
        default:                interpreter->error = PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE; return false;
        }
        /* clang-format on */
        break;
    case PS_TYPE_REAL:
        /* clang-format off */
        switch (token_type)
        {
        case PS_TOKEN_MINUS:    result->data.r = -value->data.r; break;
        default:                interpreter->error = PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE; return false;
        }
        /* clang-format on */
        break;
    case PS_TYPE_BOOLEAN:
        /* clang-format off */
        switch (token_type)
        {
        case PS_TOKEN_NOT:      result->data.b = !value->data.b; break;
        default:                interpreter->error = PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE; return false;
        }
        /* clang-format on */
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_TYPE_MISMATCH;
        return false;
    }
    return true;
}

bool ps_function_binary_op(ps_interpreter *interpreter, ps_value *a, ps_value *bb, ps_value *result, ps_token_type token_type)
{
    ps_value b;
    if (a->type->base == bb->type->base)
    {
        b.type = bb->type;
        b.data = bb->data;
    }
    else
    {
        // "cast" integer to unsigned or integer to unsigned?
        if ((a->type->base == PS_TYPE_INTEGER && bb->type->base == PS_TYPE_UNSIGNED) ||
            (a->type->base == PS_TYPE_UNSIGNED && bb->type->base == PS_TYPE_INTEGER))
        {
            b.type = a->type;
            if (!ps_function_copy_value(interpreter, bb, &b))
                return false;
        }
        else
        {
            interpreter->error = PS_RUNTIME_ERROR_TYPE_MISMATCH;
            return false;
        }
        result->type = a->type;
    }
    if (result->type == NULL || result->type->base == PS_TYPE_NONE)
        result->type = a->type;
    switch (a->type->base)
    {
    case PS_TYPE_INTEGER:
        /* clang-format off */
        switch (token_type)
        {
        case PS_TOKEN_AND:              result->data.i = a->data.i &  b.data.i; break;
        case PS_TOKEN_OR:               result->data.i = a->data.i |  b.data.i; break;
        case PS_TOKEN_XOR:              result->data.i = a->data.i ^  b.data.i; break;
        case PS_TOKEN_PLUS:             result->data.i = a->data.i +  b.data.i; break;
        case PS_TOKEN_MINUS:            result->data.i = a->data.i -  b.data.i; break;
        case PS_TOKEN_STAR:             result->data.i = a->data.i *  b.data.i; break;
        case PS_TOKEN_DIV:              result->data.i = a->data.i /  b.data.i; break;
        case PS_TOKEN_MOD:              result->data.i = a->data.i %  b.data.i; break;
        case PS_TOKEN_LESS_THAN:        result->data.i = a->data.i <  b.data.i; break;
        case PS_TOKEN_LESS_OR_EQUAL:    result->data.i = a->data.i <= b.data.i; break;
        case PS_TOKEN_GREATER_THAN:     result->data.i = a->data.i >  b.data.i; break;
        case PS_TOKEN_GREATER_OR_EQUAL: result->data.i = a->data.i >= b.data.i; break;
        case PS_TOKEN_EQUAL:            result->data.i = a->data.i == b.data.i; break;
        case PS_TOKEN_NOT_EQUAL:        result->data.i = a->data.i != b.data.i; break;
        default:                        interpreter->error = PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE; return false;
        }
        /* clang-format on */
        break;
    case PS_TYPE_UNSIGNED:
        /* clang-format off */
        switch (token_type)
        {
        case PS_TOKEN_AND:              result->data.u = a->data.u &  b.data.u; break;
        case PS_TOKEN_OR:               result->data.u = a->data.u |  b.data.u; break;
        case PS_TOKEN_XOR:              result->data.u = a->data.u ^  b.data.u; break;
        case PS_TOKEN_PLUS:             result->data.u = a->data.u +  b.data.u; break;
        case PS_TOKEN_MINUS:            result->data.u = a->data.u -  b.data.u; break;
        case PS_TOKEN_STAR:             result->data.u = a->data.u *  b.data.u; break;
        case PS_TOKEN_DIV:              result->data.u = a->data.u /  b.data.u; break;
        case PS_TOKEN_MOD:              result->data.u = a->data.u %  b.data.u; break;
        case PS_TOKEN_LESS_THAN:        result->data.u = a->data.u <  b.data.u; break;
        case PS_TOKEN_LESS_OR_EQUAL:    result->data.u = a->data.u <= b.data.u; break;
        case PS_TOKEN_GREATER_THAN:     result->data.u = a->data.u >  b.data.u; break;
        case PS_TOKEN_GREATER_OR_EQUAL: result->data.u = a->data.u >= b.data.u; break;
        case PS_TOKEN_EQUAL:            result->data.u = a->data.u == b.data.u; break;
        case PS_TOKEN_NOT_EQUAL:        result->data.u = a->data.u != b.data.u; break;
        default:                        interpreter->error = PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE; return false;
        }
        /* clang-format on */
        break;
    case PS_TYPE_REAL:
        /* clang-format off */
        switch (token_type)
        {
        case PS_TOKEN_PLUS:             result->data.r = a->data.r +  b.data.r; break;
        case PS_TOKEN_MINUS:            result->data.r = a->data.r -  b.data.r; break;
        case PS_TOKEN_STAR:             result->data.r = a->data.r *  b.data.r; break;
        case PS_TOKEN_SLASH:            result->data.r = a->data.r /  b.data.r; break;
        case PS_TOKEN_LESS_THAN:        result->data.b = (ps_boolean)(a->data.r <  b.data.r); break;
        case PS_TOKEN_LESS_OR_EQUAL:    result->data.b = (ps_boolean)(a->data.r <= b.data.r); break;
        case PS_TOKEN_GREATER_THAN:     result->data.b = (ps_boolean)(a->data.r >  b.data.r); break;
        case PS_TOKEN_GREATER_OR_EQUAL: result->data.b = (ps_boolean)(a->data.r >= b.data.r); break;
        case PS_TOKEN_EQUAL:            result->data.b = (ps_boolean)(a->data.r == b.data.r); break;
        case PS_TOKEN_NOT_EQUAL:        result->data.b = (ps_boolean)(a->data.r != b.data.r); break;
        default:                        interpreter->error = PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE; return false;
        }
        /* clang-format on */
        break;
    case PS_TYPE_BOOLEAN:
        /* clang-format off */
        switch (token_type)
        {
        case PS_TOKEN_AND:              result->data.b = (ps_boolean)(a->data.b && b.data.b); break;
        case PS_TOKEN_OR:               result->data.b = (ps_boolean)(a->data.b || b.data.b); break;
        case PS_TOKEN_XOR:              result->data.b = (ps_boolean)(a->data.b != b.data.b); break;
        default:                        interpreter->error = PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE; return false;
        }
        /* clang-format on */
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_TYPE_MISMATCH;
        return false;
    }
    return true;
}

bool ps_function_exec(ps_interpreter *interpreter, ps_symbol *symbol, ps_value *value, ps_value *result)
{
    ps_function_1arg function = symbol->value->data.v;
    if (function== NULL)
    {
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED; // PS_UNKNOWN_FUNCTION;
        return false;
    }
    if (!function(interpreter, value, result))
    {
        return false;
    }
    return true;
}

/******************************************************************************/
/* ORDINAL                                                                    */
/******************************************************************************/

/** @brief ODD - true if integer/unsigned value is odd, false if even */
bool ps_function_odd(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    result->type = ps_system_boolean.value->type;
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
        result->data.b = (ps_boolean)((value->data.u & 1) != 0);
        break;
    case PS_TYPE_INTEGER:
        result->data.b = (ps_boolean)((value->data.i & 1) != 0);
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return false;
    }
    return true;
}

/** @brief EVEN - true if integer/unsigned value is even, false if odd */
bool ps_function_even(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    if (!ps_function_odd(interpreter, value, result))
        return false;
    result->data.b = !result->data.b;
    return true;
}

/** @brief ORD - Get ordinal value of boolean / char */
bool ps_function_ord(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
        // case PS_TYPE_ENUM:
        result->type = ps_system_unsigned.value->type;
        result->data.u = value->data.u;
        break;
    case PS_TYPE_INTEGER:
        // case PS_TYPE_SUBRANGE:
        result->type = ps_system_integer.value->type;
        result->data.u = value->data.u;
        break;
    case PS_TYPE_BOOLEAN:
        // ord(false) => 0 / ord(true) => 1
        result->type = ps_system_integer.value->type;
        result->data.i = value->data.b ? 1 : 0;
        break;
    case PS_TYPE_CHAR:
        // ord('0') => 48 / ord('A') => 65 / ...
        result->type = ps_system_integer.value->type;
        result->data.i = (ps_integer)(value->data.c);
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return false;
    }
    return true;
}

/** @brief CHR - Get char value of unsigned / integer or subrange value */
bool ps_function_chr(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    result->type = ps_system_char.value->data.t;
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
        if (interpreter->range_check && value->data.u > PS_CHAR_MAX)
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        result->data.c = (ps_char)(value->data.u);
        break;
    case PS_TYPE_INTEGER:
        if (interpreter->range_check && (value->data.i < 0 || value->data.i > PS_CHAR_MAX))
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        result->data.c = (ps_char)(value->data.i);
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return false;
    }
    return true;
}

/** @brief PRED - Get previous value (predecessor) of scalar value */
bool ps_function_pred(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    result->type = value->type;
    switch (value->type->base)
    {
    case PS_TYPE_INTEGER:
        // pred(min) => error / pred(i) => i - 1
        if (interpreter->range_check && value->data.i == PS_INTEGER_MIN)
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        result->data.i = value->data.i - 1;
        break;
    // case PS_TYPE_SUBRANGE:
    //   TODO needs low()
    case PS_TYPE_UNSIGNED:
        // pred(0) => error / pred(u) => u - 1
        if (interpreter->range_check && value->data.u == 0)
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        result->data.u = value->data.u - 1;
        break;
    // case PS_TYPE_ENUM:
    //   TODO needs low()
    case PS_TYPE_BOOLEAN:
        // pred(true) => false / pred(false) => error
        if (interpreter->range_check && value->data.b == false)
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        // this will make pred(false) = false
        result->data.b = (ps_boolean) false;
        break;
    case PS_TYPE_CHAR:
        // pred(NUL) => error / pred(c) => c - 1
        if (interpreter->range_check && value->data.c == 0)
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        result->data.c = value->data.c - 1;
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return false;
    }
    return true;
}

/** @brief SUCC - Get next value (successor) of ordinal value */
bool ps_function_succ(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    result->type = value->type;
    switch (value->type->base)
    {
    case PS_TYPE_INTEGER:
        // succ(max) => error / succ(i) => i + 1
        if (interpreter->range_check && value->data.u == PS_INTEGER_MAX)
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        result->data.i = value->data.i + 1;
        break;
    // case PS_TYPE_SUBRANGE:
    //   TODO needs high()
    case PS_TYPE_UNSIGNED:
        // succ(max) => error / succ(u) => u + 1
        if (interpreter->range_check && value->data.u == PS_UNSIGNED_MAX)
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        result->data.u = value->data.u + 1;
        break;
    // case PS_TYPE_ENUM:
    //   TODO needs high()
    case PS_TYPE_BOOLEAN:
        // succ(true) => error / succ(false) => true
        if (interpreter->range_check && value->data.b == true)
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        result->data.b = true;
        break;
    case PS_TYPE_CHAR:
        // succ(char_max) => error / succ(c) => c + 1
        if (interpreter->range_check && value->data.c == PS_CHAR_MAX)
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        result->data.c = value->data.c + 1;
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return false;
    }
    return true;
}

/******************************************************************************/
/* "MATH"                                                                     */
/******************************************************************************/

bool ps_function_random(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    if (value == NULL)
    {
        // no argument, return random real between 0.0 and 1.0 excluded
        result->type = ps_system_real.value->data.t;
        do
        {
            result->data.r = (double)rand() / (double)RAND_MAX;
        } while (result->data.r >= 1.0);
    }
    else
    {
        // one argument, return random integer / unsigned
        switch (value->type->base)
        {
        case PS_TYPE_INTEGER:
            result->type = ps_system_integer.value->data.t;
            result->data.i = rand() % value->data.i;
            break;
        case PS_TYPE_UNSIGNED:
            result->type = ps_system_unsigned.value->data.t;
            result->data.u = rand() % value->data.u;
            break;
        default:
            interpreter->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
            return false;
        }
    }
    return true;
}

/** @brief ABS - Get absolute value of integer / unsigned / real */
bool ps_function_abs(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    result->type = value->type;
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
        // abs(u) => u
        result->data.u = value->data.u;
        break;
    case PS_TYPE_INTEGER:
        result->data.i = abs(value->data.i);
        break;
    case PS_TYPE_REAL:
        result->data.r = fabs(value->data.r);
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_EXPECTED_NUMBER;
        return false;
    }
    return true;
}

/** @brief TRUNC - Truncate real as integer */
bool ps_function_trunc(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->base)
    {
    case PS_TYPE_REAL:
        if (interpreter->range_check && (value->data.r < PS_INTEGER_MIN || value->data.r > PS_INTEGER_MAX))
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        result->type = ps_system_integer.value->type;
        result->data.i = (ps_integer)trunc(value->data.r);
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_EXPECTED_REAL;
        return false;
    }
    return true;
}

/** @brief ROUND - Round real as integer */
bool ps_function_round(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->base)
    {
    case PS_TYPE_REAL:
        if (interpreter->range_check && (value->data.r < PS_INTEGER_MIN || value->data.r > PS_INTEGER_MAX))
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        result->type = ps_system_integer.value->type;
        result->data.i = (ps_integer)round(value->data.r);
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_EXPECTED_REAL;
        return false;
    }
    return true;
}

/** @brief INT - Get integer part of floating point value */
bool ps_function_int(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->base)
    {
    case PS_TYPE_REAL:
        double r;
        result->type = ps_system_real.value->type;
        modf(value->data.r, &r);
        result->data.r = r;
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_EXPECTED_REAL;
        return false;
    }
    return true;
}

/** @brief FRAC - Get fractional part of floating point value */
bool ps_function_frac(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    double int_part;
    switch (value->type->base)
    {
    case PS_TYPE_REAL:
        result->type = ps_system_real.value->type;
        result->data.r = modf(value->data.r, &int_part);
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_EXPECTED_REAL;
        return false;
    }
    return true;
}
