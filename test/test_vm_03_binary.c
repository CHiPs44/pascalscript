/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>

#include "../src/vm.h"
#include "../src/operator.h"

vm_t _vm;
vm_t *vm = &_vm;

#include "../src/symbol_table.c"
#include "../src/symbol_stack.c"
#include "../src/vm.c"
#include "../src/operator.c"

int main(void)
{
    int result;
    error_t code;
    printf("TEST VM #03 BINARY: BEGIN\n");
    vm_init(vm);
    symbol_table_dump(&vm->globals, "Init");
    printf("TEST VM #03 BINARY: INIT OK\n");
    symbol_t symbol = {"I", KIND_VARIABLE, TYPE_INTEGER, sizeof(PS_INTEGER), {0}};
    result = vm_global_add(vm, &symbol);
    symbol_table_dump(&vm->globals, "VAR I: INTEGER;");
    printf("TEST VM #03 BINARY: VAR I: INTEGER; %s %d\n", result == 0 ? "OK" : "KO", result);
    vm_stack_push(vm, vm_global_get(vm, "I"));
    symbol_t *n1234 = vm_auto_add_int(vm, 1234);
    symbol_t *n5678 = vm_auto_add_int(vm, 5678);
    vm_stack_push(vm, n1234);
    vm_stack_push(vm, n5678);
    symbol_stack_dump(&vm->stack, "3 PUSH?");
    code = vm_exec_op_binary(vm, OP_ADD);
    printf("TEST VM #03 BINARY: 6912; %s %d\n", code == ERROR_NONE ? "OK" : "KO", code);
    code = vm_exec_assign(vm);
    vm_auto_gc(vm);
    symbol_stack_dump(&vm->stack, "3 POP?");
    symbol_table_dump(&vm->globals, "I=6912?");
    printf("TEST VM #03 BINARY: I := 1234 + 5678 = 6912; %s %d\n", code == ERROR_NONE ? "OK" : "KO", code);
    printf("TEST VM #03 BINARY: END\n");
    return 0;
}
