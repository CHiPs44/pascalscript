/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
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
#define PS_VALUE_STACK_SIZE (256UL)
#endif

#define PS_VALUE_STACKERROR_OVERFLOW -2

    typedef struct _value_stack_t
    {
        size_t sp;
        ps_value *values[PS_VALUE_STACK_SIZE];
    } ps_value_stack;

    // clang-format off
    ps_value_stack *ps_value_stack_alloc(void);
    size_t          ps_value_stack_size(ps_value_stack *stack);
    bool            ps_value_stack_full(ps_value_stack *stack);
    bool            ps_value_stack_push(ps_value_stack *stack, ps_value *symbol);
    ps_value       *ps_value_stack_pop (ps_value_stack *stack);
    bool            ps_value_stack_poke(ps_value_stack *stack, ps_value *symbol);
    ps_value       *ps_value_stack_peek(ps_value_stack *stack);
    void            ps_value_stack_dump(ps_value_stack *stack, char *title);
    // clang-format on

#ifdef __cplusplus
}
#endif

#endif /* _PS_VALUE_STACK_H */
