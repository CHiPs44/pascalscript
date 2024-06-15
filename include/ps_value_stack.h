/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_VALUE_STACK_H
#define _PS_VALUE_STACK_H

#include <stdbool.h>

#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef VALUE_STACKSIZE
#define VALUE_STACKSIZE 256
#endif

#define VALUE_STACKERROR_OVERFLOW -2

    typedef struct _value_stack_t
    {
        int sp;
        ps_value_t *values[VALUE_STACKSIZE];
    } ps_value_stack_t;

    void ps_value_stack_init(ps_value_stack_t *stack);
    int ps_value_stack_size(ps_value_stack_t *stack);
    bool ps_value_stack_full(ps_value_stack_t *stack);
    void ps_value_stack_dump(ps_value_stack_t *stack, char *title);
    int ps_value_stack_push(ps_value_stack_t *stack, ps_value_t *symbol);
    ps_value_t *ps_value_stack_pop(ps_value_stack_t *stack);
    bool ps_value_stack_poke(ps_value_stack_t *stack, ps_value_t *symbol);
    ps_value_t *ps_value_stack_peek(ps_value_stack_t *stack);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VALUE_STACK_H */
