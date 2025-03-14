/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "ps_error.h"
#include "ps_value.h"
#include "ps_runtime.h"

/** @brief Create new runtime */
ps_runtime *ps_vm_init_runtime()
{
    ps_runtime *runtime = calloc(1, sizeof(ps_runtime));
    if (runtime == NULL)
        return NULL;
    runtime->error = PS_RUNTIME_ERROR_NONE;
    runtime->range_check = false;
    return runtime;
}

/** @brief Create new runtime */
ps_runtime *ps_runtime_free(ps_runtime *runtime)
{
    free(runtime);
}

/** @brief Allocate new value */
ps_value *ps_runtime_alloc_value(ps_runtime *runtime)
{
    if (runtime->error != PS_RUNTIME_ERROR_NONE)
        return NULL;
    ps_value *value = (ps_value *)calloc(1, sizeof(ps_value));
    if (value == NULL)
    {
        runtime->error = PS_RUNTIME_ERROR_OUT_OF_MEMORY;
        return NULL;
    }
    return value;
}

/** @brief Free value */
void ps_runtime_free_value(ps_runtime *runtime, ps_value *value)
{
    free(value);
}

/******************************************************************************/
/* FUNCTIONS                                                                  */
/******************************************************************************/

/** @brief Get absolute value of integer / unsigned / real */
ps_value *ps_runtime_func_abs(ps_runtime *runtime, ps_value *value)
{
    if (!ps_value_is_number(value))
    {
        runtime->error = PS_RUNTIME_ERROR_EXPECTED_NUMBER;
        return NULL;
    }
    ps_value *result = ps_runtime_alloc_value(runtime);
    if (result == NULL)
    {
        runtime->error = PS_RUNTIME_ERROR_OUT_OF_MEMORY;
        return NULL;
    }
    memcpy(result, value, sizeof(ps_value));
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
        // abs(u) => u
        return result;
    case PS_TYPE_INTEGER:
        result->data.i = abs(value->data.i);
        return result;
    case PS_TYPE_REAL:
        result->data.r = fabs(value->data.r);
        return result;
    default:
        free(result);
        runtime->error = PS_RUNTIME_ERROR_EXPECTED_NUMBER;
        return NULL;
    }
}

/** @brief true if integer/unsigned value is odd, false if even */
ps_value *ps_runtime_func_odd(ps_runtime *runtime, ps_value *value)
{
    ps_value *result = ps_runtime_alloc_value(runtime);
    if (result == NULL)
        return NULL;
    result->type = PS_TYPE_BOOLEAN;
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
        result->data.b = (ps_boolean)(value->data.u & 1 == 1);
        return result;
    case PS_TYPE_INTEGER:
        result->data.b = (ps_boolean)(value->data.i & 1 == 1);
        return result;
    default:
        free(result);
        runtime->error = PS_RUNTIME_ERROR_EXPECTED_INTEGER_OR_UNSIGNED;
        return NULL;
    }
}

/** @brief true if integer/unsigned value is even, false if odd */
ps_value *ps_runtime_func_even(ps_runtime *runtime, ps_value *value)
{
    ps_value *result = ps_runtime_func_odd(runtime, value);
    if (result == NULL)
        return NULL;
    result->data.b = !result->data.b;
    return result;
}

/** @brief Get ordinal value of boolean / char */
ps_value *ps_runtime_func_ord(ps_runtime *runtime, ps_value *value)
{
    ps_value *result = ps_runtime_alloc_value(runtime);
    if (result == NULL)
        return NULL;
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
    case PS_TYPE_INTEGER:
    case PS_TYPE_ENUM:
    case PS_TYPE_SUBRANGE:
        // just copy: ord(x) => x
        memcpy(result, value, sizeof(ps_value));
        return result;
    case PS_TYPE_BOOLEAN:
        // ord(true) => 1 / ord(false) => 0
        result->type = PS_TYPE_UNSIGNED;
        result->data.u = value->data.b ? 1u : 0u;
        return result;
    case PS_TYPE_CHAR:
        // ord('0') => 48 / ord('A') => 65 / ...
        result->type = PS_TYPE_UNSIGNED;
        result->data.u = (ps_unsigned)(value->data.c);
        return result;
    default:
        free(result);
        runtime->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return NULL;
    }
}

/** @brief Get char value of unsigned / integer or subrange value */
ps_value *ps_runtime_func_chr(ps_runtime *runtime, ps_value *value)
{
    if (!ps_value_is_scalar(value))
    {
        runtime->error = PS_RUNTIME_ERROR_EXPECTED_SCALAR;
        return NULL;
    }
    ps_value *result = ps_runtime_alloc_value(runtime);
    if (result == NULL)
        return NULL;
    result->type->base = PS_TYPE_CHAR;
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
    case PS_TYPE_ENUM:
        result->data.c = (ps_char)(value->data.u);

    case PS_TYPE_INTEGER:
    case PS_TYPE_SUBRANGE:
        result->data.c = (ps_char)(value->data.i);
        return result;
    default:
        free(result);
        runtime->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return NULL;
    }
    return result;
}

/** @brief Get previous value (predecessor) of scalar value */
ps_value *ps_runtime_func_pred(ps_runtime *runtime, ps_value *value)
{
    if (value == NULL)
    {
        runtime->error = PS_RUNTIME_ERROR_EXPECTED_VALUE;
        return NULL;
    }
    if (!ps_value_is_scalar(value))
    {
        runtime->error = PS_RUNTIME_ERROR_TYPE_MISMATCH;
        return NULL;
    }
    ps_value *result = ps_runtime_alloc_value(runtime);
    if (result == NULL)
        return NULL;
    memcpy(result, value, sizeof(ps_value));
    switch (value->type->base)
    {
    case PS_TYPE_INTEGER:
        // succ(min) => error / succ(i) => i - 1
        if (runtime->range_check && value->data.i == ps_integer_min)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.i = value->data.i - 1;
        return result;
    case PS_TYPE_UNSIGNED:
        // pred(0) => error / pred(u) => u - 1
        if (runtime->range_check && value->data.u == 0)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.u = value->data.u - 1;
        return result;
    // case PS_TYPE_ENUM:
    //   TODO needs low()
    // case PS_TYPE_SUBRANGE:
    //   TODO needs low()
    case PS_TYPE_BOOLEAN:
        // succ(true) => false / succ(false) => error
        if (runtime->range_check && value->data.b == false)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.b = false;
        return result;
    case PS_TYPE_CHAR:
        // succ(NUL) => error / succ(c) => c - 1
        if (runtime->range_check && value->data.c == 0)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.c = value->data.c - 1;
        return result;
    default:
        free(result);
        runtime->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return NULL;
    }
}

/** @brief Get next value (successor) of ordinal value */
ps_value *ps_runtime_func_succ(ps_runtime *runtime, ps_value *value)
{
    ps_value *result = ps_runtime_alloc_value(runtime);
    if (result == NULL)
        return NULL;
    memcpy(result, value, sizeof(ps_value));
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
        // succ(max) => error / succ(u) => u + 1
        if (runtime->range_check && value->data.u == ps_unsigned_max)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.u = value->data.u + 1;
        return result;
    // case PS_TYPE_ENUM:
    //   TODO needs high()
    case PS_TYPE_INTEGER:
        // succ(max) => error / succ(i) => i + 1
        if (runtime->range_check && value->data.u == ps_integer_max)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.i = value->data.i + 1;
        return result;
    // case PS_TYPE_SUBRANGE:
    //   TODO needs high()
    case PS_TYPE_BOOLEAN:
        // succ(true) => error / succ(false) => true
        if (runtime->range_check && value->data.b == true)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.b = true;
        return result;
    case PS_TYPE_CHAR:
        // succ(char_max) => error / succ(c) => c + 1
        if (runtime->range_check && value->data.c == ps_char_max)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.c = value->data.c + 1;
        return result;
    default:
        free(result);
        runtime->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return NULL;
    }
}
