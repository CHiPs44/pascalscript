/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_VALUE_STACK_H
#define _PS_VALUE_STACK_H

#include <stdbool.h>

#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef PS_VALUE_STACK_SIZE
#define PS_VALUE_STACK_SIZE (1024UL)
#endif

#define PS_VALUE_STACK_SIZE_MAX UINT32_MAX

    typedef uint32_t ps_value_stack_size;

    typedef struct s_ps_value_stack
    {
        ps_value_stack_size sp;               // Stack pointer
        ps_value values[PS_VALUE_STACK_SIZE]; // Values
        ps_error error;
    } ps_value_stack;

    // clang-format off
    ps_value_stack  *ps_value_stack_init(ps_value_stack *stack);
    void             ps_value_stack_done(ps_value_stack *stack);
    void             ps_value_stack_dump(ps_value_stack *stack, char *title);
    ps_value_stack_size           ps_value_stack_get_size(ps_value_stack *stack);
    bool             ps_value_stack_is_full(ps_value_stack *stack);
    /** @brief Push to stack if not full */
    bool             ps_value_stack_push(ps_value_stack *stack, ps_value *value);
    /** @brief Pop top of stack if any */
    ps_value        *ps_value_stack_pop (ps_value_stack *stack);
    /** @brief Replace top of stack if any */
    bool             ps_value_stack_poke(ps_value_stack *stack, ps_value *value);
    /** @brief Read top of stack if any */
    ps_value        *ps_value_stack_peek(ps_value_stack *stack);
    // clang-format on

#ifdef __cplusplus
}
#endif

#endif /* _PS_VALUE_STACK_H */
