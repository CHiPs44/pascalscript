/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_stack.h"
#include "ps_memory.h"
#include "ps_value_data.h"

ps_frame *ps_frame_alloc(size_t size)
{
    // NB: works even if size is 0
    ps_frame *frame = ps_memory_malloc(PS_MEMORY_ENVIRONMENT, sizeof(ps_frame) + size * sizeof(ps_value_data));
    if (frame == NULL)
        return NULL;
    frame->size = size;
    return frame;
}

ps_frame *ps_frame_free(ps_frame *frame)
{
    ps_memory_free(PS_MEMORY_ENVIRONMENT, frame);
    return NULL;
}

ps_stack *ps_stack_alloc(size_t size)
{
    ps_stack *stack = ps_memory_malloc(PS_MEMORY_ENVIRONMENT, sizeof(ps_stack) + size * sizeof(ps_frame *));
    if (stack == NULL)
        return NULL;
    stack->size = size;
    stack->sp = 0;
    return stack;
}

ps_stack *ps_stack_free(ps_stack *stack)
{
    for (size_t i = 0; i < stack->sp; i++)
        stack->frames[i] = ps_frame_free(stack->frames[i]);
    ps_memory_free(PS_MEMORY_ENVIRONMENT, stack);
    return NULL;
}

ps_frame *ps_stack_push(ps_stack *stack, ps_frame *frame)
{
    if (stack->sp >= stack->size)
        return NULL;
    stack->frames[stack->sp++] = frame;
    return frame;
}

ps_frame *ps_stack_pop(ps_stack *stack)
{
    if (stack->sp == 0)
        return NULL;
    stack->sp -= 1;
    ps_frame *frame = stack->frames[stack->sp];
    stack->frames[stack->sp] = NULL;
    return frame;
}

ps_frame *ps_stack_top(ps_stack *stack)
{
    if (stack->sp == 0)
        return NULL;
    return stack->frames[stack->sp - 1];
}

bool ps_stack_is_empty(ps_stack *stack)
{
    return stack->sp == 0;
}

bool ps_stack_is_full(ps_stack *stack)
{
    return stack->sp == stack->size;
}
