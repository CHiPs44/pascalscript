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

    ps_value *ps_runtime_alloc_value(ps_runtime *runtime);
    void ps_runtime_free_value(ps_runtime *runtime, ps_value *value);

    /** @brief Get absolute value of integer / unsigned / real */
    ps_value *ps_runtime_func_abs(ps_runtime *runtime, ps_value *value);

    /** @brief true if integer/unsigned value is odd, false if even */
    ps_value *ps_runtime_func_odd(ps_runtime *runtime, ps_value *value);

    /** @brief true if integer/unsigned value is even, false if odd */
    ps_value *ps_runtime_func_even(ps_runtime *runtime, ps_value *value);

    /** @brief Get ordinal value of boolean / char */
    ps_value *ps_runtime_func_ord(ps_runtime *runtime, ps_value *value);

    /** @brief Get char value of unsigned / integer or subrange value */
    ps_value *ps_runtime_func_chr(ps_runtime *runtime, ps_value *value);

    /** @brief Get previous value of ordinal value */
    ps_value *ps_runtime_func_pred(ps_runtime *runtime, ps_value *value);

    /** @brief Get next value of ordinal value */
    ps_value *ps_runtime_func_succ(ps_runtime *runtime, ps_value *value);

    // ps_runtime_func_sizeof?

#ifdef __cplusplus
}
#endif

#endif /* _PS_RUNTIME_H */
