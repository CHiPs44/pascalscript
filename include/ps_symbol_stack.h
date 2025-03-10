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
#define PS_SYMBOL_STACK_SIZE (256UL)
#endif

#define PS_SYMBOL_STACK_ERROR_OVERFLOW (-1)

    typedef struct s_ps_symbol_stack
    {
        size_t sp;
        ps_symbol *symbols[PS_SYMBOL_STACK_SIZE];
    } ps_symbol_stack;

#define PS_SYMBOL_STACK_SIZE sizeof(ps_symbol_stack)

    // clang-format off
    ps_symbol_stack *ps_symbol_stack_init(ps_symbol_stack *stack);
    void             ps_symbol_stack_free(ps_symbol_stack *stack);
    void             ps_symbol_stack_dump(ps_symbol_stack *stack, char *title);
    size_t           ps_symbol_stack_size(ps_symbol_stack *stack);
    bool             ps_symbol_stack_full(ps_symbol_stack *stack);
    /** @brief Push to stack if not full */
    bool             ps_symbol_stack_push(ps_symbol_stack *stack, ps_symbol *symbol);
    /** @brief Pop top of stack if any */
    ps_symbol       *ps_symbol_stack_pop (ps_symbol_stack *stack);
    /** @brief Replace top of stack if any */
    bool             ps_symbol_stack_poke(ps_symbol_stack *stack, ps_symbol *symbol);
    /** @brief Read top of stack if any */
    ps_symbol       *ps_symbol_stack_peek(ps_symbol_stack *stack);
    // clang-format on

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_STACK_H */
