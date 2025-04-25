/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_FUNCTIONS_H
#define _PS_FUNCTIONS_H

#include <stdio.h>

#include "ps_interpreter.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef bool (*ps_function_1arg)(ps_interpreter *interpreter, ps_value *value, ps_value *result);

    /**
     *  @brief Copy value of "from" into "to", converting unsigned to integer and vice versa
     *         sets error to PS_RUNTIME_ERROR_OUT_OF_RANGE or PS_RUNTIME_ERROR_TYPE_MISMATCH
     */
    bool ps_function_copy_value(ps_interpreter *interpreter, ps_value *from, ps_value *to);

    bool ps_function_unary_op(ps_interpreter *interpreter, ps_value *value, ps_value *result, ps_token_type token_type);

    bool ps_function_binary_op(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result, ps_token_type token_type);

    /* clang-format off */

    bool ps_function_write_text (ps_interpreter *interpreter, FILE *f, ps_value *value);
    bool ps_function_randomize  (ps_interpreter *interpreter);

    bool ps_function_odd        (ps_interpreter *interpreter, ps_value *value, ps_value *result);
    bool ps_function_even       (ps_interpreter *interpreter, ps_value *value, ps_value *result);
    bool ps_function_ord        (ps_interpreter *interpreter, ps_value *value, ps_value *result);
    bool ps_function_chr        (ps_interpreter *interpreter, ps_value *value, ps_value *result);
    bool ps_function_pred       (ps_interpreter *interpreter, ps_value *value, ps_value *result);
    bool ps_function_succ       (ps_interpreter *interpreter, ps_value *value, ps_value *result);
    bool ps_function_random     (ps_interpreter *interpreter, ps_value *value, ps_value *result);
    bool ps_function_abs        (ps_interpreter *interpreter, ps_value *value, ps_value *result);
    bool ps_function_trunc      (ps_interpreter *interpreter, ps_value *value, ps_value *result);
    bool ps_function_round      (ps_interpreter *interpreter, ps_value *value, ps_value *result);
    bool ps_function_int        (ps_interpreter *interpreter, ps_value *value, ps_value *result);
    bool ps_function_frac       (ps_interpreter *interpreter, ps_value *value, ps_value *result);

    /* clang-format on */

#ifdef __cplusplus
}
#endif

#endif /* _PS_FUNCTIONS_H */
