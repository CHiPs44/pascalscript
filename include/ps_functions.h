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

    bool ps_functions_init(ps_environment *system);

    extern ps_symbol ps_system_function_abs;
    extern ps_symbol ps_system_function_abs;
    extern ps_symbol ps_system_function_arctan;
    extern ps_symbol ps_system_function_chr;
    extern ps_symbol ps_system_function_cos;
    extern ps_symbol ps_system_function_even;
    extern ps_symbol ps_system_function_exp;
    extern ps_symbol ps_system_function_frac;
    extern ps_symbol ps_system_function_get_tick_count;
    extern ps_symbol ps_system_function_high;
    extern ps_symbol ps_system_function_int;
    extern ps_symbol ps_system_function_length;
    extern ps_symbol ps_system_function_ln;
    extern ps_symbol ps_system_function_log;
    extern ps_symbol ps_system_function_low;
    extern ps_symbol ps_system_function_lowercase;
    extern ps_symbol ps_system_function_odd;
    extern ps_symbol ps_system_function_ord;
    extern ps_symbol ps_system_function_power;
    extern ps_symbol ps_system_function_pred;
    extern ps_symbol ps_system_function_random;
    extern ps_symbol ps_system_function_round;
    extern ps_symbol ps_system_function_sin;
    extern ps_symbol ps_system_function_sqr;
    extern ps_symbol ps_system_function_sqrt;
    extern ps_symbol ps_system_function_succ;
    extern ps_symbol ps_system_function_tan;
    extern ps_symbol ps_system_function_trunc;
    extern ps_symbol ps_system_function_uppercase;

    bool ps_function_unary_op(ps_interpreter *interpreter, ps_value *value, ps_value *result, ps_token_type token_type);

    bool ps_function_binary_op(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result,
                               ps_token_type token_type);

    void ps_operator_binary_dump(void);

    /* clang-format off */

  ps_error ps_function_exec_1arg      (ps_interpreter *interpreter, ps_symbol *symbol, ps_value *value, ps_value *result);
  ps_error ps_function_abs            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_arctan         (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_chr            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_cos            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_even           (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_exp            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_frac           (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_get_tick_count (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_int            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_length         (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_ln             (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_log            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_lowercase      (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_odd            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_ord            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_pred           (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_random         (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_round          (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_sin            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_sqr            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_sqrt           (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_succ           (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_tan            (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_trunc          (ps_interpreter *interpreter, ps_value *value, ps_value *result);
  ps_error ps_function_uppercase      (ps_interpreter *interpreter, ps_value *value, ps_value *result);

  ps_error ps_function_exec_1arg_s    (ps_interpreter *interpreter, ps_symbol *symbol, ps_symbol *type, ps_value *result);
  ps_error ps_function_high           (ps_interpreter *interpreter, ps_symbol *type, ps_value *result);
  ps_error ps_function_low            (ps_interpreter *interpreter, ps_symbol *type, ps_value *result);

  ps_error ps_function_exec_2args     (ps_interpreter *interpreter, ps_symbol *symbol, ps_value *a, ps_value *b, ps_value *result);
  ps_error ps_function_power          (ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result);

    /* clang-format on */

#ifdef __cplusplus
}
#endif

#endif /* _PS_FUNCTIONS_H */
