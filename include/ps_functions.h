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

    typedef ps_error (*ps_function_1arg)(ps_interpreter *interpreter, ps_value *value, ps_value *result);

    bool ps_function_unary_op(ps_interpreter *interpreter, ps_value *value, ps_value *result, ps_token_type token_type);

    bool ps_function_binary_op(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result,
                               ps_token_type token_type);

    void ps_operator_binary_dump();

    /* clang-format off */

  ps_error ps_function_exec           (ps_interpreter *interpreter, ps_symbol *symbol, ps_value *value, ps_value *result);

  ps_error ps_function_chr            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_even           (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_odd            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_ord            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_pred           (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_succ           (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  
  ps_error ps_function_abs            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_arctan         (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_cos            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_exp            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_frac           (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_int            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_ln             (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_log            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_round          (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_sin            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_sqr            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_sqrt           (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_tan            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_trunc          (ps_interpreter *interpreter, ps_value *value, ps_value *result);

  ps_error ps_function_length         (ps_interpreter *interpreter, ps_value *value, ps_value *result);

  ps_error ps_function_get_tick_count (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_random         (ps_interpreter *interpreter, ps_value *value, ps_value *result);

    /* clang-format on */

#ifdef __cplusplus
}
#endif

#endif /* _PS_FUNCTIONS_H */
