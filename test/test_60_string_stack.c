/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "../include/ps_string.h"
#include "../include/ps_string_stack.h"

int main(void)
{
    ps_string_stack *stack = ps_string_stack_alloc(2);
    ps_string *first = ps_string_create("first", 5);
    ps_string *second = ps_string_create("second", 6);
    if (stack == NULL || first == NULL || second == NULL)
    {
        ps_string_stack_free(stack, false);
        ps_string_free(first);
        ps_string_free(second);
        return 1;
    }

    bool ok = ps_string_stack_is_empty(stack) && !ps_string_stack_is_full(stack);
    bool first_on_stack = false;
    bool second_on_stack = false;
    first_on_stack = ps_string_stack_push(stack, first) == first;
    ok = ok && first_on_stack;
    second_on_stack = ps_string_stack_push(stack, second) == second;
    ok = ok && second_on_stack;
    ok = ok && ps_string_stack_is_full(stack) && ps_string_stack_top(stack) == second;

    errno = 0;
    ok = ok && ps_string_stack_push(stack, first) == NULL && errno == EOVERFLOW;
    const ps_string *popped = ps_string_stack_pop(stack);
    if (popped == second)
        second_on_stack = false;
    ok = ok && popped == second;
    popped = ps_string_stack_pop(stack);
    if (popped == first)
        first_on_stack = false;
    ok = ok && popped == first;
    ok = ok && ps_string_stack_is_empty(stack) && ps_string_stack_pop(stack) == NULL;

    ps_string_stack_free(stack, true);
    if (!first_on_stack)
        ps_string_free(first);
    if (!second_on_stack)
        ps_string_free(second);
    printf("TEST STRING STACK %s\n", ok ? "OK" : "FAILED!");
    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}
