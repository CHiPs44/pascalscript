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

    static ps_error ps_errno = PS_ERROR_ZERO;

    inline ps_value *ps_abs(ps_value *value)
    {
        ps_value *result = (ps_value *)calloc(1, sizeof(ps_value));
        if (result == NULL)
        {
            ps_errno = PS_RUNTIME_ERROR_OUT_OF_MEMORY;
            return NULL;
        }
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
            ps_errno = PS_RUNTIME_ERROR_EXPECTED_NUMBER;
            return NULL;
        }
    }

    inline ps_value *ps_odd(ps_value *value)
    {
        ps_value *result = (ps_value *)calloc(1, sizeof(ps_value));
        if (result == NULL)
        {
            ps_errno = PS_RUNTIME_ERROR_OUT_OF_MEMORY;
            return NULL;
        }
        result->type = PS_TYPE_BOOLEAN;
        result->size = sizeof(ps_boolean);
        switch (value->type)
        {
        case PS_TYPE_UNSIGNED:
            result->data.b = (ps_boolean)(value->data.u & 1u);
            return result;
        case PS_TYPE_INTEGER:
            result->data.b = (ps_boolean)(value->data.i & 1u);
            return result;
        default:
            free(result);
            ps_errno = PS_RUNTIME_ERROR_EXPECTED_INTEGER_OR_UNSIGNED;
            return NULL;
        }
    }

    inline ps_value *ps_even(ps_value *value)
    {
        ps_value *result = ps_odd(value);
        if (result == NULL)
            return NULL;
        result->data.b = !result->data.b;
        return result;
    }

    // ps_ord
    // ps_chr

#ifdef __cplusplus
}
#endif

#endif /* _PS_RUNTIME_H */
