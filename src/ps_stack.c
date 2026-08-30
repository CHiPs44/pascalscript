/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_memory.h"
#include "ps_stack.h"
#include "ps_value_data.h"

ps_frame *ps_frame_alloc(const ps_ast_block *block)
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
    for (size_t i = 0; i < count; i++)
        frame->data[i].u = i % 2 == 0 ? 0xDEADDEAD : 0xBEEFBEEF;
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

ps_frame *ps_stack_find_frame_for_block(const ps_stack *stack, const ps_ast_block *block)
{
    if (ps_stack_is_empty(stack))
        return NULL;
    for (size_t i = stack->sp - 1; i > 0; --i)
        if (stack->frames[i]->block == block)
            return stack->frames[i];
    return NULL;
}

void ps_stack_dump(FILE *output, const ps_stack *stack)
{
    fprintf(output, "Stack: %zu/%zu\n", stack->used, stack->size);
    fprintf(output, "  SP: %zu\n", stack->sp);
    for (size_t i = 0; i < stack->sp; i++)
    {
        ps_frame *frame = stack->frames[i];
        fprintf(output, "  Frame: %p\n", (void *)frame);
        fprintf(output, "    Block: %s %s\n", ps_ast_node_get_kind_name(frame->block->kind), frame->block->name);
        fprintf(output, "    Variables: %d\n", frame->block->n_vars);
        for (ps_handle handle = 0; handle < frame->block->n_vars; handle++)
        {
            ps_symbol *symbol = ps_symbol_table_find_variable_by_handle(frame->block->symbols, handle);
            if (symbol == NULL)
            {
                fprintf(output, "      %d: NOT FOUND!\n", handle);
                continue;
            }
            ps_value value = {.allocated = false, .type = symbol->value->type, .data = frame->data[handle]};
            fprintf(output, "      %d: %s = %s\n", handle, symbol->name, ps_value_get_debug_string(&value));
        }
    }
}
