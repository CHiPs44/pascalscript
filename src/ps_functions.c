/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <math.h>
#include <string.h>

#include "ps_error.h"
#include "ps_system.h"
#include "ps_value.h"
#include "ps_interpreter.h"
#include "ps_functions.h"

/******************************************************************************/
/* BASE                                                                       */
/******************************************************************************/

bool ps_function_not(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    result->type = value->type;
    // NB: with FPC, not(subrange) or not(enum) yields integer result without range checking
    switch (value->type->base)
    {
    case PS_TYPE_INTEGER:
        result->data.i = ~value->data.i;
        break;
    case PS_TYPE_UNSIGNED:
        result->data.u = ~value->data.u;
        break;
    case PS_TYPE_BOOLEAN:
        result->data.b = !value->data.b;
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_TYPE_MISMATCH;
        return false;
    }
    return true;
}

#define OP_AND 1
#define OP_OR 2
#define OP_XOR 3

bool ps_function_and_or_xor(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result, int op)
{
    if (a->type->base != b->type->base)
    {
        interpreter->error = PS_RUNTIME_ERROR_TYPE_MISMATCH;
        return false;
    }
    result->type = a->type;
    switch (a->type->base)
    {
    case PS_TYPE_INTEGER:
        switch (op)
        {
        case OP_AND:
            result->data.i = a->data.i & b->data.i;
            break;
        case OP_OR:
            result->data.i = a->data.i | b->data.i;
            break;
        case OP_XOR:
            result->data.i = a->data.i ^ b->data.i;
            break;
        }
        break;
    case PS_TYPE_UNSIGNED:
        switch (op)
        {
        case OP_AND:
            result->data.u = a->data.u & b->data.u;
            break;
        case OP_OR:
            result->data.u = a->data.u | b->data.u;
            break;
        case OP_XOR:
            result->data.u = a->data.u ^ b->data.u;
            break;
        }
        break;
    case PS_TYPE_BOOLEAN:
        switch (op)
        {
        case OP_AND:
            result->data.b = (ps_boolean)(a->data.b && b->data.b);
            break;
        case OP_OR:
            result->data.b = (ps_boolean)(a->data.b || b->data.b);
            break;
        case OP_XOR:
            result->data.b = (ps_boolean)(a->data.b != b->data.b);
            break;
        }
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_TYPE_MISMATCH;
        return false;
    }
    return true;
}

bool ps_function_and(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)
{
    return ps_function_and_or_xor(interpreter, a, b, result, OP_AND);
}

bool ps_function_or(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)
{
    return ps_function_and_or_xor(interpreter, a, b, result, OP_OR);
}

bool ps_function_xor(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)
{
    return ps_function_and_or_xor(interpreter, a, b, result, OP_XOR);
}

/******************************************************************************/
/* ORDINAL                                                                    */
/******************************************************************************/

bool ps_function_neg(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    result->type = value->type;
    switch (value->type->base)
    {
    case PS_TYPE_INTEGER:
        result->data.i = -value->data.i;
        break;
    case PS_TYPE_REAL:
        result->data.r = -value->data.r;
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_EXPECTED_NUMBER;
        return false;
    }
    return true;
}

bool ps_function_odd(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    result->type = ps_symbol_boolean.value->type;
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
        result->data.b = (ps_boolean)(value->data.u & 1 == 1);
        break;
    case PS_TYPE_INTEGER:
        result->data.b = (ps_boolean)(value->data.i & 1 == 1);
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return false;
    }
}

bool ps_function_even(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    if (!ps_function_odd(interpreter, value, result))
        return false;
    result->data.b = !result->data.b;
    return true;
}

bool ps_function_ord(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
        // case PS_TYPE_ENUM:
        result->type = ps_symbol_unsigned.value->type;
        result->data.u = value->data.u;
        break;
    case PS_TYPE_INTEGER:
        // case PS_TYPE_SUBRANGE:
        result->type = ps_symbol_integer.value->type;
        result->data.u = value->data.u;
        break;
    case PS_TYPE_BOOLEAN:
        // ord(false) => 0 / ord(true) => 1
        result->type = ps_symbol_integer.value->type;
        result->data.i = value->data.b ? 1 : 0;
        break;
    case PS_TYPE_CHAR:
        // ord('0') => 48 / ord('A') => 65 / ...
        result->type = ps_symbol_integer.value->type;
        result->data.i = (ps_integer)(value->data.c);
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return false;
    }
}

bool ps_function_chr(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    result->type = &ps_symbol_char.value->type;
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

bool ps_function_trunc(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    result->type = ps_symbol_integer.value->type;
    switch (value->type->base)
    {
    case PS_TYPE_REAL:
        if (interpreter->range_check && (value->data.r < PS_INTEGER_MIN || value->data.r > PS_INTEGER_MAX))
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        result->data.i = (ps_integer)trunc(value->data.r);
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_EXPECTED_REAL;
        return false;
    }
    return true;
}

bool ps_function_round(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    result->type = ps_symbol_integer.value->type;
    switch (value->type->base)
    {
    case PS_TYPE_REAL:
        if (interpreter->range_check && (value->data.r < PS_INTEGER_MIN || value->data.r > PS_INTEGER_MAX))
        {
            interpreter->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return false;
        }
        result->data.i = (ps_integer)round(value->data.r);
        break;
    default:
        interpreter->error = PS_RUNTIME_ERROR_EXPECTED_REAL;
        return false;
    }
    return true;
}
