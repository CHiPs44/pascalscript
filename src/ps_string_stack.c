/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>
#include <stdint.h>

#include "ps_memory.h"
#include "ps_string_stack.h"

ps_string_stack *ps_string_stack_alloc(size_t size)
{
    ps_string_stack *stack = ps_memory_malloc(PS_MEMORY_STACK, sizeof(ps_string_stack) + size * sizeof(ps_string *));
    if (stack == NULL)
        return NULL;
    stack->size = size;
    stack->sp = 0;
    return stack;
}

ps_string_stack *ps_string_stack_free(ps_string_stack *stack, bool free_strings)
{
    if (stack == NULL)
        return NULL;
    if (free_strings)
        for (size_t i = 0; i < stack->sp; i++)
        {
            if (stack->strings[i] != NULL)
                stack->strings[i] = ps_string_free(stack->strings[i]);
        }
    ps_memory_free(PS_MEMORY_STACK, stack);
    return NULL;
}

bool ps_string_stack_is_empty(const ps_string_stack *stack) { return stack == NULL || stack->sp == 0; }

bool ps_string_stack_is_full(const ps_string_stack *stack) { return stack == NULL || stack->sp >= stack->size; }

ps_string *ps_string_stack_push(ps_string_stack *stack, ps_string *string)
{
    if (string == NULL)
    {
        errno = EINVAL;
        return NULL;
    }
    if (ps_string_stack_is_full(stack))
    {
        errno = EOVERFLOW;
        return NULL;
    }
    stack->strings[stack->sp++] = string;
    return string;
}

ps_string *ps_string_stack_pop(ps_string_stack *stack)
{
    if (ps_string_stack_is_empty(stack))
        return NULL;
    ps_string *string = stack->strings[--stack->sp];
    stack->strings[stack->sp] = NULL;
    return string;
}

ps_string *ps_string_stack_top(const ps_string_stack *stack)
{
    if (ps_string_stack_is_empty(stack))
        return NULL;
    return stack->strings[stack->sp - 1];
}

void ps_string_stack_dump(FILE *output, const ps_string_stack *stack)
{
    if (output == NULL)
        output = stderr;
    if (stack == NULL)
    {
        fprintf(output, "String Stack: (null)\n");
        return;
    }
    fprintf(output, "String Stack: %zu/%zu\n", stack->sp, stack->size);
    fprintf(output, "  SP: %zu\n", stack->sp);
    for (size_t i = 0; i < stack->sp; i++)
    {
        ps_string *string = stack->strings[i];
        fprintf(output, "  %zu: %p", i, (void *)string);
        if (string != NULL)
            fprintf(output, " \"%s\"", (char *)string->str);
        fputc('\n', output);
    }
}
