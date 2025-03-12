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

ps_symbol_stack *ps_symbol_stack_init(ps_symbol_stack *stack)
{
    if (stack == NULL)
    {
        stack = calloc(1, sizeof(ps_symbol_stack));
        if (stack == NULL)
            return NULL;
    }
    /* use SIZE_MAX for an empty stack */
    stack->sp = SIZE_MAX;
    for (int i = 0; i < PS_SYMBOL_STACK_SIZE; i++)
    {
        stack->symbols[i] = NULL;
    }
    return stack;
}

void ps_symbol_stack_free(ps_symbol_stack *stack)
{
    free(stack);
}

size_t ps_symbol_stack_size(ps_symbol_stack *stack)
{
    return stack->sp == SIZE_MAX ? 0 : stack->sp + 1;
}

bool ps_symbol_stack_full(ps_symbol_stack *stack)
{
    return ps_symbol_stack_size(stack) >= PS_SYMBOL_STACK_SIZE;
}

bool ps_symbol_stack_push(ps_symbol_stack *stack, ps_symbol *symbol)
{
    if (ps_symbol_stack_full(stack))
    {
        return false;
    }
    stack->sp += 1;
    stack->symbols[stack->sp] = symbol;
    return true;
}

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

bool ps_symbol_stack_poke(ps_symbol_stack *stack, ps_symbol *symbol)
{
    if (ps_symbol_stack_size(stack) == 0)
    {
        return false;
    }
    stack->symbols[stack->sp] = symbol;
    return true;
}

ps_symbol *ps_symbol_stack_peek(ps_symbol_stack *stack)
{
    if (ps_symbol_stack_size(stack) == 0)
    {
        return NULL;
    }
    return stack->symbols[stack->sp];
}

void ps_symbol_stack_dump(ps_symbol_stack *stack, char *title)
{
    ps_symbol *symbol;
    fprintf(stderr, "*** Symbol stack %s (%d)%s ***\n",
            title,
            ps_symbol_stack_size(stack),
            ps_symbol_stack_full(stack) ? " (FULL)" : "");
    fprintf(stderr, "┏━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
    // fprintf(stderr, "┃ # ┃Name                           ┃Kind    ┃Scope   ┃Type    ┃Value                          ┃\n");
    fprintf(stderr, "┃ # ┃%-*s┃%-8s┃%-8s┃%-8s┃%8lu┃%-*s┃\n",
            PS_IDENTIFIER_MAX, "Name", "Kind", "Scope", "Type", PS_IDENTIFIER_MAX, "Value");
    fprintf(stderr, "┣━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
    for (int i = 0; i < PS_SYMBOL_STACK_SIZE; i++)
    {
        symbol = stack->symbols[i];
        if (symbol != NULL)
        {
            char *kind_name = ps_symbol_get_kind_name(symbol->kind);
            char *scope_name = ps_symbol_get_scope_name(symbol->scope);
            char *type_name = ps_value_get_type_name(symbol->value.type);
            char *buffer = ps_value_get_debug_value(&symbol->value);
            fprintf(stderr, "┃%03d┃%-*s┃%-8s┃%-8s┃%-8s┃%8lu┃%-*s┃\n",
                    i, PS_IDENTIFIER_MAX, symbol->name, kind_name, scope_name, type_name, PS_IDENTIFIER_MAX, buffer);
        }
    }
    fprintf(stderr, "┗━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━┻━━━━━━━━┻━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
}

/* EOF */
