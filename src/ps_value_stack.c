/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_system.h"
#include "ps_value.h"
#include "ps_value_stack.h"

ps_value_stack *ps_value_stack_alloc()
{
    ps_value_stack *stack = calloc(1, sizeof(ps_value_stack));
    if (stack == NULL)
        return NULL;
    /* use SIZE_MAX for an empty stack */
    stack->sp = SIZE_MAX;
    for (int i = 0; i < PS_VALUE_STACK_SIZE; i++)
    {
        stack->values[i].type = NULL;
        stack->values[i].data.v = NULL;
    }
    return stack;
}

ps_value_stack *ps_value_stack_free(ps_value_stack *stack)
{
    free(stack);
    return NULL;
}

ps_value_stack_size ps_value_stack_get_size(ps_value_stack *stack)
{
    return stack->sp == PS_VALUE_STACK_SIZE_MAX ? 0 : stack->sp + 1;
}

bool ps_value_stack_full(ps_value_stack *stack)
{
    return ps_value_stack_get_size(stack) >= PS_VALUE_STACK_SIZE;
}

bool ps_value_stack_push(ps_value_stack *stack, ps_value *value)
{
    if (ps_value_stack_full(stack))
        return false;
    stack->sp += 1;
    stack->values[stack->sp].type = value->type;
    stack->values[stack->sp].data = value->data;
    return true;
}

bool ps_value_stack_pop(ps_value_stack *stack, ps_value *value)
{
    if (ps_value_stack_get_size(stack) == 0)
        return false;
    value->type = stack->values[stack->sp].type;
    value->data = stack->values[stack->sp].data;
    stack->values[stack->sp].type = NULL;
    stack->values[stack->sp].data.v = NULL;
    stack->sp -= 1;
    return true;
}

bool ps_value_stack_poke(ps_value_stack *stack, ps_value *value)
{
    if (ps_value_stack_get_size(stack) == 0)
        return false;
    stack->values[stack->sp].type = value->type;
    stack->values[stack->sp].data = value->data;
    return true;
}

bool ps_value_stack_peek(ps_value_stack *stack, ps_value *value)
{
    if (ps_value_stack_get_size(stack) == 0)
        return false;
    value->type = stack->values[stack->sp].type;
    value->data = stack->values[stack->sp].data;
    return true;
}

void ps_value_stack_dump(ps_value_stack *stack, char *title)
{
    fprintf(stderr, "*** Value stack %s (%u)%s ***\n", title, ps_value_stack_get_size(stack),
            ps_value_stack_full(stack) ? " (FULL)" : "");
    fprintf(stderr, "┏━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
    fprintf(stderr, "┃  #  ┃%-*s┃%-*s┃\n", PS_IDENTIFIER_LEN, "Name", PS_IDENTIFIER_LEN, "Value");
    fprintf(stderr, "┣━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
    for (int i = 0; i < PS_VALUE_STACK_SIZE; i++)
    {
        ps_value value = stack->values[i];
        if (value.type != NULL)
        {
            char *buffer = ps_value_get_debug_value(&value);
            fprintf(stderr, "┃%05d┃%-*s┃%-*s┃\n", i, PS_IDENTIFIER_LEN, value.type->name, PS_IDENTIFIER_LEN, buffer);
        }
    }
    fprintf(stderr, "┗━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
}

/* EOF */
