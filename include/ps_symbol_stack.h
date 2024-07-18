/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_SYMBOL_STACK_H
#define _PS_SYMBOL_STACK_H

#include <stdbool.h>

#include "ps_symbol.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef PS_SYMBOL_STACK_SIZE
#define PS_SYMBOL_STACK_SIZE (256)
#endif

#define PS_SYMBOL_STACK_ERROR_OVERFLOW (-1)

    typedef struct _ps_symbol_stack
    {
        int sp;
        ps_symbol *symbols[PS_SYMBOL_STACK_SIZE];
    } ps_symbol_stack;

    // clang-format off
    void       ps_symbol_stack_init(ps_symbol_stack *stack);
    int        ps_symbol_stack_size(ps_symbol_stack *stack);
    bool       ps_symbol_stack_full(ps_symbol_stack *stack);
    void       ps_symbol_stack_dump(ps_symbol_stack *stack, char *title);
    int        ps_symbol_stack_push(ps_symbol_stack *stack, ps_symbol *symbol);
    ps_symbol *ps_symbol_stack_pop (ps_symbol_stack *stack);
    bool       ps_symbol_stack_poke(ps_symbol_stack *stack, ps_symbol *symbol);
    ps_symbol *ps_symbol_stack_peek(ps_symbol_stack *stack);
    // clang-format on

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_STACK_H */
