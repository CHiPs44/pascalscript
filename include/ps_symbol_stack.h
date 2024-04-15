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

#ifndef SYMBOL_STACK_SIZE
#define SYMBOL_STACK_SIZE 256
#endif

#define SYMBOL_STACK_ERROR_OVERFLOW -2

typedef struct _symbol_stack_t
{
    int sp;
    symbol_t *symbols[SYMBOL_STACK_SIZE];
} symbol_stack_t;

void      symbol_stack_init(symbol_stack_t *stack);
int       symbol_stack_size(symbol_stack_t *stack);
bool      symbol_stack_full(symbol_stack_t *stack);
void      symbol_stack_dump(symbol_stack_t *stack, char *title);
int       symbol_stack_push(symbol_stack_t *stack, symbol_t *symbol);
symbol_t *symbol_stack_pop (symbol_stack_t *stack);
int       symbol_stack_poke(symbol_stack_t *stack, symbol_t *symbol);
symbol_t *symbol_stack_peek(symbol_stack_t *stack);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_STACK_H */
