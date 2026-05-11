/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_OPERATOR_H
#define _PS_OPERATOR_H

#include <stdio.h>

#include "ps_interpreter.h"
#include "ps_value.h"
#include "ps_token.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Unary operators */
    typedef enum e_ps_operator_unary
    {
        PS_OP_UNARY_INVALID = 0,
        PS_OP_NEG,
        PS_OP_NOT,
    } ps_operator_unary;

    // clang-format off
    /** @brief Binary operators */
    typedef enum e_ps_operator_binary
    {
        PS_OP_BINARY_INVALID = 0,
        // additive / terms
        PS_OP_ADD, PS_OP_SUB, PS_OP_OR, PS_OP_XOR,
        // multiply / factors
        PS_OP_MUL, PS_OP_DIV, PS_OP_DIV_REAL, PS_OP_MOD, PS_OP_AND, PS_OP_SHL, PS_OP_SHR,
        // comparison
        PS_OP_EQ, PS_OP_GE, PS_OP_GT, PS_OP_LE, PS_OP_LT, PS_OP_NE,
    } ps_operator_binary;
    // clang-format on

    /** @brief Execute unary operation */
    bool ps_operator_unary_eval(ps_interpreter *interpreter, const ps_value *value, ps_value *result,
                                ps_operator_unary operator);
    /** @brief Execute binary operation */
    bool ps_operator_eval_binary(ps_interpreter *interpreter, const ps_value *a, const ps_value *b, ps_value *result,
                                 ps_operator_binary operator);

    ps_operator_unary ps_operator_unary_from_token(ps_token_type token_type);
    ps_operator_binary ps_operator_binary_from_token(ps_token_type token_type);
    char *ps_operator_unary_get_name(ps_operator_unary operator);
    char *ps_operator_binary_get_name(ps_operator_binary operator);

#ifdef __cplusplus
}
#endif

#endif /* _PS_OPERATOR_H */
