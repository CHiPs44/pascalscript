/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_ast.h"
#include "ps_memory.h"
#include "ps_stack.h"
#include "ps_value_data.h"

ps_frame *ps_frame_alloc(ps_ast_block *block, ps_frame *parent)
{
    // NB: works even if procedure hasn't any variables or parameters
    size_t count = block->n_vars;
    if (block->signature != NULL)
    {
        count += block->signature->parameter_count;
        if (block->signature->result_type != NULL)
            count += 1;
    }
    size_t size = sizeof(ps_frame) + count * sizeof(ps_value_data);
    ps_frame *frame = ps_memory_malloc(PS_MEMORY_STACK, size);
    if (frame == NULL)
        return NULL;
    memset(frame, 0, size);
    frame->block = block;
    frame->parent = parent;
    return frame;
}

ps_frame *ps_frame_free(ps_frame *frame)
{
    // NB: block is not freed here, it is owned by the AST
    ps_memory_free(PS_MEMORY_STACK, frame);
    return NULL;
}

ps_stack *ps_stack_alloc(size_t size)
{
    ps_stack *stack = ps_memory_malloc(PS_MEMORY_STACK, sizeof(ps_stack) + size * sizeof(ps_frame *));
    if (stack == NULL)
        return NULL;
    stack->size = size;
    stack->used = 0;
    stack->sp = 0;
    return stack;
}

ps_stack *ps_stack_free(ps_stack *stack)
{
    for (size_t i = 0; i < stack->sp; i++)
        if (stack->frames[i] != NULL)
            stack->frames[i] = ps_frame_free(stack->frames[i]);
    ps_memory_free(PS_MEMORY_STACK, stack);
    return NULL;
}

bool ps_stack_is_empty(const ps_stack *stack)
{
    return stack->used == 0;
}

bool ps_stack_is_full(const ps_stack *stack)
{
    return stack->used == stack->size;
}

ps_frame *ps_stack_push(ps_stack *stack, ps_frame *frame)
{
    if (ps_stack_is_full(stack))
        return NULL; // stack overflow
    stack->used += 1;
    stack->frames[stack->sp++] = frame;
    return frame;
}

ps_frame *ps_stack_pop(ps_stack *stack)
{
    if (ps_stack_is_empty(stack))
        return NULL; // stack underflow
    stack->sp -= 1;
    ps_frame *frame = stack->frames[stack->sp];
    stack->frames[stack->sp] = NULL;
    stack->used -= 1;
    return frame;
}

ps_frame *ps_stack_top(const ps_stack *stack)
{
    if (ps_stack_is_empty(stack))
        return NULL;
    return stack->frames[stack->sp - 1];
}
