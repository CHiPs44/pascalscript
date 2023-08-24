/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>

#include "../src/vm.h"

vm_t vm_data;
vm_t *vm = &vm_data;

#include "../src/symbol_table.c"
#include "../src/symbol_stack.c"
#include "../src/vm.c"

int main(void)
{
    int result;
    error_t code;

    printf("TEST VM #01 ASSIGN: BEGIN\n");
    vm_init(vm);
    symbol_table_dump(&vm->globals, "Init");
    printf("TEST VM #01 ASSIGN: INIT OK\n");

    symbol_t variable_i = {"I", KIND_VARIABLE, TYPE_INTEGER, sizeof(PS_INTEGER), {0}};
    result = vm_global_add(vm, &variable_i);
    printf("TEST VM #01 ASSIGN: VAR I: INTEGER; %s %d\n", result == 0 ? "OK" : "KO", result);
    symbol_table_dump(&vm->globals, "VAR I: INTEGER;");

    symbol_t *three = vm_auto_add_int(vm, 3);
    vm_stack_push(vm, vm_global_get(vm, "I"));
    vm_stack_push(vm, three);
    symbol_stack_dump(&vm->stack, "2 PUSH?");
    code = vm_exec_assign(vm);
    vm_auto_gc(vm);
    symbol_stack_dump(&vm->stack, "2 POP?");
    symbol_table_dump(&vm->globals, "I=3?");
    printf("TEST VM #01 ASSIGN: I := 3; %s %d\n", code == ERROR_NONE ? "OK" : "KO", code);

    symbol_t constant_k = {"K", KIND_CONSTANT, TYPE_INTEGER, sizeof(PS_INTEGER), {1234}};
    result = vm_global_add(vm, &constant_k);
    printf("TEST VM #01 ASSIGN: CONST K = 1234; %s %d\n", result == 0 ? "OK" : "KO", result);
    symbol_table_dump(&vm->globals, "CONST K = 1234;");

    symbol_t *_5678 = vm_auto_add_int(vm, 5678);
    vm_stack_push(vm, vm_global_get(vm, "K"));
    vm_stack_push(vm, _5678);
    symbol_stack_dump(&vm->stack, "2 PUSH?");
    code = vm_exec_assign(vm);
    vm_auto_gc(vm);
    symbol_stack_dump(&vm->stack, "2 POP?");
    symbol_table_dump(&vm->globals, "K=1234?");
    printf("TEST VM #01 ASSIGN: K := 5678; %s %d\n", code == RUNTIME_TYPE_MISMATCH ? "OK" : "KO", code);

    printf("TEST VM #01 ASSIGN: END\n");
    return 0;
}
