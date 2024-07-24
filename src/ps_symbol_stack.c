/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_value.h"
#include "ps_symbol_stack.h"

/**
 * @brief Initialize stack
 *
 * @param stack
 */
void ps_symbol_stack_init(ps_symbol_stack *stack)
{
    stack->sp = -1;
    for (int i = 0; i < PS_SYMBOL_STACK_SIZE; i++)
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
int ps_symbol_stack_size(ps_symbol_stack *stack)
{
    return stack->sp + 1;
}

/**
 * @brief Check if stack is full
 *
 * @param Stack
 * @return true if stack is full
 */
extern bool ps_symbol_stack_full(ps_symbol_stack *stack)
{
    return ps_symbol_stack_size(stack) >= PS_SYMBOL_STACK_SIZE;
}

void ps_symbol_stack_dump(ps_symbol_stack *stack, char *title)
{
    ps_symbol *symbol;
    fprintf(stderr, "*** Symbol stack %s (%d)%s ***\n",
            title,
            ps_symbol_stack_size(stack),
            ps_symbol_stack_full(stack) ? " (FULL)" : "");
    fprintf(stderr, "┏━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
    fprintf(stderr, "┃ # ┃Name                           ┃Kind    ┃Scope   ┃Type    ┃Size    ┃Value                          ┃\n");
    fprintf(stderr, "┣━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
    for (int i = 0; i < PS_SYMBOL_STACK_SIZE; i++)
    {
        if (stack->symbols[i] != NULL)
        {
            symbol = stack->symbols[i];
            // fprintf(stderr, "|%03d|%-*s|%4d|%4d|%12d|%08x|\n",
            //         i, PS_SYMBOL_NAME_MAX, symbol->name, symbol->kind, symbol->value.type, symbol->value.data.i, symbol->value.data.i);
            char *kind_name = ps_symbol_get_type_name(symbol->kind);
            char *scope_name = symbol_get_scope_name(symbol->scope);
            char *type_name = ps_value_get_type_name(symbol->value.type);
            char *buffer = ps_value_get_value(&symbol->value);
            fprintf(stderr, "┃%03d┃%-*s┃%-8s┃%-8s┃%-8s┃%8lu┃%-*s┃\n",
                    i, PS_SYMBOL_NAME_MAX, symbol->name, kind_name, scope_name, type_name, symbol->value.size, PS_SYMBOL_NAME_MAX, buffer);
        }
    }
    fprintf(stderr, "┗━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━┻━━━━━━━━┻━━━━━━━━┻━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
}

/**
 * @brief Push symbol on top of stack
 *
 * @param stack
 * @param symbol
 * @return int
 */
int ps_symbol_stack_push(ps_symbol_stack *stack, ps_symbol *symbol)
{
    if (ps_symbol_stack_full(stack))
    {
        return PS_SYMBOL_STACK_ERROR_OVERFLOW;
    }
    stack->sp += 1;
    stack->symbols[stack->sp] = symbol;
    return stack->sp;
}

/**
 * @brief Pop symbol from top of stack
 *
 * @param stack
 * @return ps_symbol* NULL if stack is empty
 */
ps_symbol *ps_symbol_stack_pop(ps_symbol_stack *stack)
{
    ps_symbol *symbol;
    if (ps_symbol_stack_size(stack) == 0)
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
 * @return true (false if stack is empty)
 */
bool ps_symbol_stack_poke(ps_symbol_stack *stack, ps_symbol *symbol)
{
    if (ps_symbol_stack_size(stack) == 0)
    {
        return false;
    }
    stack->symbols[stack->sp] = symbol;
    return true;
}

/**
 * @brief Get top of stack
 *
 * @param Stack
 * @return Top or NULL if stack is empty
 */
ps_symbol *ps_symbol_stack_peek(ps_symbol_stack *stack)
{
    if (ps_symbol_stack_size(stack) == 0)
    {
        return NULL;
    }
    return stack->symbols[stack->sp];
}

/* EOF */
