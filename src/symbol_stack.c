/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "symbol_stack.h"

/**
 * @brief Initialize stack
 *
 * @param stack
 */
void symbol_stack_init(symbol_stack_t *stack)
{
    stack->sp = -1;
    for (int i = 0; i < SYMBOL_STACK_SIZE; i++)
    {
        stack->symbols[i] = NULL;
    }
}

/**
 * @brief Get actual stack size
 *
 * @param stack
 * @return int
 */
int symbol_stack_size(symbol_stack_t *stack)
{
    return stack->sp + 1;
}

/**
 * @brief Check if stack is full
 *
 * @param Stack
 * @return true if stack is full
 */
extern bool symbol_stack_full(symbol_stack_t *stack)
{
    return symbol_stack_size(stack) >= SYMBOL_STACK_SIZE;
}

void symbol_stack_dump(symbol_stack_t *stack, char *title)
{
    symbol_t *s;
    fprintf(stderr, "*** Symbol stack %s (%d)%s ***\n",
            title,
            symbol_stack_size(stack),
            symbol_stack_full(stack) ? " (FULL)" : "");
    fprintf(stderr, "#   name                            kind type value        value\n");
    fprintf(stderr, "--- ------------------------------- ---- ---- ------------ --------\n");
    for (int i = 0; i < SYMBOL_STACK_SIZE; i++)
    {
        if (stack->symbols[i] != NULL)
        {
            s = stack->symbols[i];
            fprintf(stderr, "%03d %-*s %4d %4d %12d %08x\n",
                    i, MAX_SYMBOL_NAME, s->name, s->kind, s->type, s->value.i, s->value.i);
        }
    }
}

/**
 * @brief Push symbol on top of stack
 *
 * @param stack
 * @param symbol
 * @return int
 */
int symbol_stack_push(symbol_stack_t *stack, symbol_t *symbol)
{
    if (symbol_stack_full(stack))
    {
        return SYMBOL_STACK_ERROR_OVERFLOW;
    }
    stack->sp += 1;
    stack->symbols[stack->sp] = symbol;
    return stack->sp;
}

/**
 * @brief Pop symbol from top of stack
 *
 * @param stack
 * @return symbol_t* NULL if stack is empty
 */
symbol_t *symbol_stack_pop(symbol_stack_t *stack)
{
    symbol_t *symbol;
    if (symbol_stack_size(stack) == 0)
    {
        return NULL;
    }
    symbol = stack->symbols[stack->sp];
    stack->symbols[stack->sp] = NULL;
    stack->sp -= 1;
    return symbol;
}

/**
 * @brief Replace top of stack
 *
 * @param Stack
 * @param Symbol
 * @return SP (-1 if stack is empty)
 */
int symbol_stack_poke(symbol_stack_t *stack, symbol_t *symbol)
{
    if (symbol_stack_size(stack) == 0)
    {
        return stack->sp;
    }
    stack->symbols[stack->sp] = symbol;
    return stack->sp;
}

/**
 * @brief Get top of stack
 *
 * @param Stack
 * @return Top or NULL if stack is empty
 */
symbol_t *symbol_stack_peek(symbol_stack_t *stack)
{
    if (symbol_stack_size(stack) == 0)
    {
        return NULL;
    }
    return stack->symbols[stack->sp];
}

/* EOF */
