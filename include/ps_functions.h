/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_FUNCTIONS_H
#define _PS_FUNCTIONS_H

#include "ps_interpreter.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief NOT - Compute bitwise not for integer / unsigned, logical not for boolean */
    bool ps_function_abs(ps_interpreter *interpreter, ps_value *value, ps_value *result);
    /** @brief AND - Compute bitwise and for integer / unsigned, logical and for boolean */
    bool ps_function_and(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result);
    /** @brief OR - Compute bitwise or for integer / unsigned, logical or for boolean */
    bool ps_function_or(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result);
    /** @brief XOR - Compute bitwise xor for integer / unsigned, logical xor for boolean */
    bool ps_function_xor(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result);

    /** @brief ODD - true if integer/unsigned value is odd, false if even */
    bool ps_function_odd(ps_interpreter *interpreter, ps_value *value, ps_value *result);
    /** @brief EVEN - true if integer/unsigned value is even, false if odd */
    bool ps_function_even(ps_interpreter *interpreter, ps_value *value, ps_value *result);

    /** @brief ORD - Get ordinal value of boolean / char */
    bool ps_function_ord(ps_interpreter *interpreter, ps_value *value, ps_value *result);
    /** @brief CHR - Get char value of unsigned / integer or subrange value */
    bool ps_function_chr(ps_interpreter *interpreter, ps_value *value, ps_value *result);

    /** @brief PRED - Get previous value (predecessor) of scalar value */
    bool ps_function_pred(ps_interpreter *interpreter, ps_value *value, ps_value *result);
    /** @brief SUCC - Get next value (successor) of ordinal value */
    bool ps_function_succ(ps_interpreter *interpreter, ps_value *value, ps_value *result);

    /** @brief NEG - Negate value of integer / unsigned / real */
    bool ps_function_neg(ps_interpreter *interpreter, ps_value *value, ps_value *result);

    /** @brief ABS - Get absolute value of integer / unsigned / real */
    bool ps_function_abs(ps_interpreter *interpreter, ps_value *value, ps_value *result);

    /** @brief TRUNC - Truncate real as integer */
    bool ps_function_trunc(ps_interpreter *interpreter, ps_value *value, ps_value *result);

    /** @brief ROUND - Round real as integer */
    bool ps_function_round(ps_interpreter *interpreter, ps_value *value, ps_value *result);

#ifdef __cplusplus
}
#endif

#endif /* _PS_FUNCTIONS_H */
