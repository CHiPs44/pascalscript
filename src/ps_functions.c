/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <string.h>

#include "ps_error.h"
#include "ps_parser.h"
#include "ps_system.h"
#include "ps_value.h"
#include "ps_interpreter.h"
#include "ps_functions.h"

bool ps_function_abs(ps_interpreter *interpreter, ps_value *value, ps_value *result)
{
    if (!ps_value_is_number(value))
    {
        interpreter->error = PS_RUNTIME_ERROR_EXPECTED_NUMBER;
        return false;
    }
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
