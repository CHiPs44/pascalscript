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
    ps_frame *frame = ps_memory_calloc(PS_MEMORY_ENVIRONMENT, size, sizeof(ps_frame) + size * sizeof(ps_value_data));
    if (frame == NULL)
        return NULL;
    frame->size = size;
    return frame;
}

ps_frame *ps_frame_free(ps_frame *frame)
{
    ps_memory_free(PS_MEMORY_ENVIRONMENT, frame);
}

ps_stack *ps_stack_alloc(size_t size)
{
    ps_stack *stack = ps_memory_calloc(PS_MEMORY_ENVIRONMENT, size, sizeof(ps_stack) + size * sizeof(ps_frame));
    if (stack == NULL)
        return NULL;
    stack->size = size;
    stack->sp = 0;
    return stack;
}

ps_stack *ps_stack_free(ps_stack *stack)
{
    ps_memory_free(PS_MEMORY_ENVIRONMENT, stack);
}

ps_frame *ps_stack_push(ps_stack *stack, ps_frame *frame)
{
    if (stack->sp >= stack->size)
        return NULL;
    stack->frames[stack->sp++] = frame;
}

ps_frame *ps_stack_pop(ps_stack *stack)
{
    if (stack->sp == 0)
        return NULL;
    return stack->frames[--stack->sp];
}
