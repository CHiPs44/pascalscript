/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>

#include "../include/ps_value.h"
#include "../include/ps_symbol.h"
// #define SYMBOL_STACK_SIZE 3
#include "../include/ps_symbol_stack.h"

#include "../src/ps_value.c"
#include "../src/ps_symbol.c"
#include "../src/ps_symbol_stack.c"

symbol_stack_t stack;
symbol_t constant1 = {.name = "CONSTANT1", .kind = KIND_CONSTANT};//, .type = PS_TYPE_INTEGER, .size = sizeof(int), .value.i = 0x0000DEAD};
symbol_t variable2 = {.name = "VARIABLE2", .kind = KIND_VARIABLE};//, .type = PS_TYPE_INTEGER, .size = sizeof(int), .value.i = 0x0000BEEF};
symbol_t constant3 = {.name = "CONSTANT3", .kind = KIND_CONSTANT};//, .type = PS_TYPE_INTEGER, .size = sizeof(int), .value.i = 0x12345678};
symbol_t constant4 = {.name = "CONSTANT4", .kind = KIND_CONSTANT};//, .type = PS_TYPE_INTEGER, .size = sizeof(int), .value.i = 0x87654321};

int main(void)
{
    symbol_t *symbol;
    int result;

    ps_value_integer(&constant1.value, 1234567890);
    ps_value_integer(&variable2.value, 0xDEADBEEF);
    ps_value_integer(&constant3.value, 0x12345678);
    ps_value_integer(&constant4.value, 0x87654321);

    printf("TEST SYMBOL STACK: BEGIN\n");
    symbol_stack_init(&stack);
    printf("TEST SYMBOL STACK: INIT OK\n");
    symbol_stack_dump(&stack, "Test");
    printf("TEST SYMBOL STACK: DUMP OK\n");
    // Pop from empty stack => NULL
    symbol = symbol_stack_pop(&stack);
    printf("TEST SYMBOL STACK: POP %s %p\n", symbol == NULL ? "OK" : "KO", symbol);
    // Push value => index 0
    result = symbol_stack_push(&stack, &constant1);
    printf("TEST SYMBOL STACK: PUSH %s %d\n", result == 0 ? "OK" : "KO", result);
    symbol_stack_dump(&stack, "Test");
    printf("TEST SYMBOL STACK: DUMP OK\n");
    // Push 3 values => indexes 1, 2 & 3
    result = symbol_stack_push(&stack, &variable2);
    printf("TEST SYMBOL STACK: PUSH %s %d\n", result == 1 ? "OK" : "KO", result);
    result = symbol_stack_push(&stack, &constant3);
    printf("TEST SYMBOL STACK: PUSH %s %d\n", result == 2 ? "OK" : "KO", result);
    result = symbol_stack_push(&stack, &constant4);
    printf("TEST SYMBOL STACK: PUSH %s %d\n", result == 3 ? "OK" : "KO", result);
    symbol_stack_dump(&stack, "Test");
    printf("TEST SYMBOL STACK: DUMP OK\n");
    // Pop 2 values => constant4 & constant3
    symbol = symbol_stack_pop(&stack);
    printf("TEST SYMBOL STACK: POP %s %p\n", symbol == &constant4 ? "OK" : "KO", symbol);
    symbol = symbol_stack_pop(&stack);
    printf("TEST SYMBOL STACK: POP %s %p\n", symbol == &constant3 ? "OK" : "KO", symbol);
    // This is the end
    symbol_stack_dump(&stack, "Test");
    printf("TEST SYMBOL STACK: DUMP OK\n");
    printf("TEST SYMBOL STACK: END\n");

    return 0;
}
