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
        return 1;

    bool ok = ps_string_stack_is_empty(stack) && !ps_string_stack_is_full(stack);
    ok = ok && ps_string_stack_push(stack, first) == first;
    ok = ok && ps_string_stack_push(stack, second) == second;
    ok = ok && ps_string_stack_is_full(stack) && ps_string_stack_top(stack) == second;

    errno = 0;
    ok = ok && ps_string_stack_push(stack, first) == NULL && errno == EOVERFLOW;
    ok = ok && ps_string_stack_pop(stack) == second && ps_string_stack_pop(stack) == first;
    ok = ok && ps_string_stack_is_empty(stack) && ps_string_stack_pop(stack) == NULL;

    ps_string_stack_free(stack);
    ps_string_free(first);
    ps_string_free(second);
    printf("TEST STRING STACK %s\n", ok ? "OK" : "KO");
    return ok ? 0 : 1;
}
