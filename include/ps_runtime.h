/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_RUNTIME_H
#define _PS_RUNTIME_H

#include <string.h>

#include "ps_error.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    // static ps_error runtime->errno = PS_ERROR_ZERO;

    typedef struct s_ps_runtime
    {
        ps_error errno;
    } ps_runtime;

    inline ps_value *ps_runtime_alloc_value(ps_runtime *runtime)
    {
        if (runtime->errno != PS_RUNTIME_ERROR_NONE)
            return NULL;
        ps_value *value = (ps_value *)calloc(1, sizeof(ps_value));
        if (value == NULL)
        {
            runtime->errno = PS_RUNTIME_ERROR_OUT_OF_MEMORY;
            return NULL;
        }
        return value;
    }

    inline void ps_runtime_free_value(ps_runtime *runtime, ps_value *value)
    {
        free(value);
    }

    /** @brief Get absolute value of integer / unsigned / real */
    inline ps_value *ps_runtime_func_abs(ps_runtime *runtime, ps_value *value)
    {
        ps_value *result = ps_runtime_alloc_value(runtime);
        if (result == NULL)
            return NULL;
        memcpy(result, value, sizeof(ps_value));
        switch (value->type)
        {
        case PS_TYPE_UNSIGNED:
            return result;
        case PS_TYPE_INTEGER:
            result->data.i = abs(value->data.i);
            return result;
        case PS_TYPE_REAL:
            result->data.r = abs(value->data.r);
            return result;
        default:
            free(result);
            runtime->errno = PS_RUNTIME_ERROR_EXPECTED_NUMBER;
            return NULL;
        }
    }

    /** @brief true if integer/unsigned value is odd, false if even */
    inline ps_value *ps_runtime_func_odd(ps_runtime *runtime, ps_value *value)
    {
        ps_value *result = ps_runtime_alloc_value(runtime);
        if (result == NULL)
            return NULL;
        result->type = PS_TYPE_BOOLEAN;
        result->size = sizeof(ps_boolean);
        switch (value->type)
        {
        case PS_TYPE_UNSIGNED:
            result->data.b = (ps_boolean)(value->data.u % 2 == 1);
            return result;
        case PS_TYPE_INTEGER:
            result->data.b = (ps_boolean)(value->data.i % 2 == 1);
            return result;
        default:
            free(result);
            runtime->errno = PS_RUNTIME_ERROR_EXPECTED_INTEGER_OR_UNSIGNED;
            return NULL;
        }
    }

    /** @brief true if integer/unsigned value is even, false if odd */
    inline ps_value *ps_runtime_func_even(ps_runtime *runtime, ps_value *value)
    {
        ps_value *result = ps_runtime_func_odd(runtime, value);
        if (result == NULL)
            return NULL;
        result->data.b = !result->data.b;
        return result;
    }

    /** @brief Get ordinal value of boolean / char */
    inline ps_value *ps_runtime_func_ord(ps_runtime *runtime, ps_value *value)
    {
        ps_value *result = ps_runtime_alloc_value(runtime);
        if (result == NULL)
            return NULL;
        switch (value->type)
        {
        case PS_TYPE_UNSIGNED:
        case PS_TYPE_INTEGER:
        case PS_TYPE_ENUM:
        case PS_TYPE_SUBRANGE:
            // just copy: ord(x) => x
            memcpy(result, value, sizeof(ps_value));
            return result;
        case PS_TYPE_BOOLEAN:
            result->type = PS_TYPE_UNSIGNED;
            result->size = sizeof(ps_unsigned);
            // ord(true) => 1 / ord(false) => 0
            result->data.u = value->data.b ? 1u : 0u;
            return result;
        case PS_TYPE_CHAR:
            result->type = PS_TYPE_UNSIGNED;
            result->size = sizeof(ps_unsigned);
            // ord('0') => 48 / ord('A') => 65 / ...
            result->data.u = (ps_unsigned)(value->data.c);
            return result;
        default:
            free(result);
            runtime->errno = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
            return NULL;
        }
    }

    /** @brief Get char value of unsigned / integer or subrange value */
    inline ps_value *ps_runtime_func_chr(ps_runtime *runtime, ps_value *value)
    {
        ps_value *result = ps_runtime_alloc_value(runtime);
        if (result == NULL)
            return NULL;
        result->type = PS_TYPE_CHAR;
        result->size = sizeof(ps_char);
        switch (value->type)
        {
        case PS_TYPE_UNSIGNED:
            result->data.c = (ps_unsigned)(value->data.u);
            return result;
        case PS_TYPE_INTEGER:
        case PS_TYPE_SUBRANGE:
            result->data.c = (ps_integer)(value->data.i);
            return result;
        default:
            free(result);
            runtime->errno = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
            return NULL;
        }
    }

    /** @brief Get previous value of ordinal value */
    inline ps_value *ps_runtime_func_pred(ps_runtime *runtime, ps_value *value)
    {
        ps_value *result = ps_runtime_alloc_value(runtime);
        if (result == NULL)
            return NULL;
        memcpy(result, value, sizeof(ps_value));
        switch (value->type)
        {
        case PS_TYPE_UNSIGNED:
            result->data.u = value->data.u - 1;
            return result;
        // case PS_TYPE_ENUM:
        //   TODO needs low()
        case PS_TYPE_INTEGER:
            result->data.u = value->data.u - 1;
            return result;
        case PS_TYPE_SUBRANGE:
        case PS_TYPE_BOOLEAN:
            result->type = PS_TYPE_UNSIGNED;
            result->size = sizeof(ps_unsigned);
            // ord(true) => 1 / ord(false) => 0
            result->data.u = value->data.b ? 1u : 0u;
            return result;
        case PS_TYPE_CHAR:
            result->type = PS_TYPE_UNSIGNED;
            result->size = sizeof(ps_unsigned);
            // ord('0') => 48 / ord('A') => 65 / ...
            result->data.u = (ps_unsigned)(value->data.c);
            return result;
        default:
            free(result);
            runtime->errno = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
            return NULL;
        }
    }

    // ps_runtime_func_succ
    // ps_runtime_func_sizeof?

#ifdef __cplusplus
}
#endif

#endif /* _PS_RUNTIME_H */
