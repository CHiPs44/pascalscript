/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>

#include "../include/ps_value.h"
#include "../include/ps_symbol_table.h"
#include "../include/ps_symbol_stack.h"
#include "../include/ps_vm.h"

ps_vm _vm;
ps_vm *vm = &_vm;

#include "../src/ps_value.c"
#include "../src/ps_symbol_table.c"
#include "../src/ps_symbol_stack.c"
#include "../src/ps_vm.c"

int main(void)
{
    int result;
    ps_error code;

    printf("TEST VM #01 ASSIGN: BEGIN\n");
    vm_init(vm);
    ps_symbol_table_dump(&vm->symbols, "Init");
    printf("TEST VM #01 ASSIGN: INIT OK\n");

    ps_symbol variable_i = {"I", SYMBOL_TYPE_VARIABLE, PS_TYPE_INTEGER, sizeof(ps_integer), {0}};
    result = vm_global_add(vm, &variable_i);
    printf("TEST VM #01 ASSIGN: VAR I: INTEGER; %s %d\n", result == 0 ? "OK" : "KO", result);
    ps_symbol_table_dump(&vm->symbols, "VAR I: INTEGER;");

    ps_symbol *three = vm_auto_add_integer(vm, 3);
    vm_stack_push(vm, vm_global_get(vm, "I"));
    vm_stack_push(vm, three);
    ps_symbol_stack_dump(&vm->stack, "2 PUSH?");
    code = vm_exec_assign(vm);
    vm_auto_gc(vm);
    ps_symbol_stack_dump(&vm->stack, "2 POP?");
    ps_symbol_table_dump(&vm->symbols, "I=3?");
    printf("TEST VM #01 ASSIGN: I := 3; %s %d\n", code == PS_ERROR_ZERO ? "OK" : "KO", code);

    ps_symbol constant_k = {"K", SYMBOL_TYPE_CONSTANT, PS_TYPE_INTEGER, sizeof(ps_integer), {1234}};
    result = vm_global_add(vm, &constant_k);
    printf("TEST VM #01 ASSIGN: CONST K = 1234; %s %d\n", result == 0 ? "OK" : "KO", result);
    ps_symbol_table_dump(&vm->symbols, "CONST K = 1234;");

    ps_symbol *_5678 = vm_auto_add_integer(vm, 5678);
    vm_stack_push(vm, vm_global_get(vm, "K"));
    vm_stack_push(vm, _5678);
    ps_symbol_stack_dump(&vm->stack, "2 PUSH?");
    code = vm_exec_assign(vm);
    vm_auto_gc(vm);
    ps_symbol_stack_dump(&vm->stack, "2 POP?");
    ps_symbol_table_dump(&vm->symbols, "K=1234?");
    printf("TEST VM #01 ASSIGN: K := 5678; %s %d\n", code == PS_RUNTIME_ERROR_TYPE_MISMATCH ? "OK" : "KO", code);

    printf("TEST VM #01 ASSIGN: END\n");
    return 0;
}
