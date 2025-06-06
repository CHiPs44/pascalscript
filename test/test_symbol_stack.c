/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>
#include <sys/resource.h>

#include "../include/ps_value.h"
#include "../include/ps_symbol.h"
// #define PS_SYMBOL_STACK_SIZE 3
#include "../include/ps_symbol_stack.h"

#include "../src/ps_value.c"
#include "../src/ps_symbol.c"
#include "../src/ps_symbol_stack.c"

ps_symbol_stack stack;
ps_symbol constant1 = {.name = "CONSTANT1", .kind = PS_SYMBOL_KIND_CONSTANT};
ps_symbol variable2 = {.name = "VARIABLE2", .kind = PS_SYMBOL_KIND_VARIABLE};
ps_symbol constant3 = {.name = "CONSTANT3", .kind = PS_SYMBOL_KIND_CONSTANT};
ps_symbol constant4 = {.name = "CONSTANT4", .kind = PS_SYMBOL_KIND_CONSTANT};
ps_symbol constant5 = {.name = "CONSTANT5", .kind = PS_SYMBOL_KIND_CONSTANT};

int main(void)
{
    struct rlimit rl = {256 * 1024 * 12, 256 * 1024 * 12};
    setrlimit(RLIMIT_AS, &rl);

    ps_symbol *symbol;
    int result;
    ps_string string5 = {.str = "ABCDEF123456", .len = 12};

    ps_value_set_integer(&constant1.value, 1234567890);
    ps_value_set_integer(&variable2.value, 0xDEADBEEF);
    ps_value_set_integer(&constant3.value, 0x12345678);
    ps_value_set_integer(&constant4.value, 0x87654321);
    ps_value_set_string(&constant5.value, string5);

    printf("TEST SYMBOL STACK: BEGIN\n");
    ps_symbol_stack_init(&stack);
    printf("TEST SYMBOL STACK: INIT OK\n");
    ps_symbol_stack_dump(&stack, "Test");
    printf("TEST SYMBOL STACK: DUMP OK\n");
    // Pop from empty stack => NULL
    symbol = ps_symbol_stack_pop(&stack);
    printf("TEST SYMBOL STACK: POP %s %p\n", symbol == NULL ? "OK" : "KO", symbol);
    // Push value => index 0
    result = ps_symbol_stack_push(&stack, &constant1);
    printf("TEST SYMBOL STACK: PUSH %s %d\n", result == 0 ? "OK" : "KO", result);
    ps_symbol_stack_dump(&stack, "Test");
    printf("TEST SYMBOL STACK: DUMP OK\n");
    // Push 3 values => indexes 1, 2 & 3
    result = ps_symbol_stack_push(&stack, &variable2);
    printf("TEST SYMBOL STACK: PUSH %s %d\n", result == 1 ? "OK" : "KO", result);
    result = ps_symbol_stack_push(&stack, &constant3);
    printf("TEST SYMBOL STACK: PUSH %s %d\n", result == 2 ? "OK" : "KO", result);
    result = ps_symbol_stack_push(&stack, &constant4);
    printf("TEST SYMBOL STACK: PUSH %s %d\n", result == 3 ? "OK" : "KO", result);
    result = ps_symbol_stack_push(&stack, &constant5);
    printf("TEST SYMBOL STACK: PUSH %s %d\n", result == 4 ? "OK" : "KO", result);
    ps_symbol_stack_dump(&stack, "Test");
    printf("TEST SYMBOL STACK: DUMP OK\n");
    // Pop 2 values => constant4 & constant3
    symbol = ps_symbol_stack_pop(&stack);
    printf("TEST SYMBOL STACK: POP %s %p\n", symbol == &constant4 ? "OK" : "KO", symbol);
    symbol = ps_symbol_stack_pop(&stack);
    printf("TEST SYMBOL STACK: POP %s %p\n", symbol == &constant3 ? "OK" : "KO", symbol);
    // This is the end
    ps_symbol_stack_dump(&stack, "Test");
    printf("TEST SYMBOL STACK: DUMP OK\n");
    printf("TEST SYMBOL STACK: END\n");

    return 0;
}
