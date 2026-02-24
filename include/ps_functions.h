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

    bool ps_function_unary_op(const ps_interpreter *interpreter, const ps_value *value, ps_value *result,
                              ps_token_type token_type);

    bool ps_function_binary_op(const ps_interpreter *interpreter, const ps_value *a, const ps_value *b,
                               ps_value *result, ps_token_type token_type);

    void ps_operator_binary_dump(void);

    /* clang-format off */

  ps_error ps_function_exec_1arg      (const ps_interpreter *interpreter, const ps_symbol *symbol, const ps_value *value, ps_value *result);
  ps_error ps_function_abs            (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_arctan         (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_chr            (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_cos            (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_even           (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_exp            (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_frac           (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_get_tick_count (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_int            (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_length         (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_ln             (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_log            (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_lowercase      (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_odd            (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_ord            (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_pred           (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_random         (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_round          (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_sin            (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_sqr            (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_sqrt           (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_succ           (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_tan            (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_trunc          (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);
  ps_error ps_function_uppercase      (const ps_interpreter *interpreter, const ps_value *value, ps_value *result);

  ps_error ps_function_exec_1arg_s    (const ps_interpreter *interpreter, const ps_symbol *symbol, const ps_symbol *type, ps_value *result);
  ps_error ps_function_high           (const ps_interpreter *interpreter, const ps_symbol *type, ps_value *result);
  ps_error ps_function_low            (const ps_interpreter *interpreter, const ps_symbol *type, ps_value *result);

  ps_error ps_function_exec_2args     (const ps_interpreter *interpreter, const ps_symbol *symbol, const ps_value *a, const ps_value *b, ps_value *result);
  ps_error ps_function_power          (const ps_interpreter *interpreter, const ps_value *a, const ps_value *b, ps_value *result);

    /* clang-format on */

#ifdef __cplusplus
}
#endif

#endif /* _PS_FUNCTIONS_H */
