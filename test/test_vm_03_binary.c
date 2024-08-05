/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <sys/resource.h>

#include "../src/ps_vm.h"
#include "../src/ps_operator.h"

ps_vm _vm;
ps_vm *vm = &_vm;

#include "../src/ps_symbol_table.c"
#include "../src/ps_symbol_stack.c"
#include "../src/ps_vm.c"
#include "../src/ps_operator.c"

int main(void)
{
    struct rlimit rl = {256 * 1024 * 12, 256 * 1024 * 12};
    setrlimit(RLIMIT_AS, &rl);

    int result;
    ps_error_t code;
    printf("TEST VM #03 BINARY: BEGIN\n");
    ps_runtime_init(vm);
    ps_symbol_table_dump(&vm->symbols, "Init");
    printf("TEST VM #03 BINARY: INIT OK\n");
    ps_symbol symbol = {"I", PS_SYMBOL_KIND_VARIABLE, PS_TYPE_INTEGER, sizeof(ps_integer), {0}};
    result = vm_global_add(vm, &symbol);
    ps_symbol_table_dump(&vm->symbols, "VAR I: INTEGER;");
    printf("TEST VM #03 BINARY: VAR I: INTEGER; %s %d\n", result == 0 ? "OK" : "KO", result);
    vm_stack_push(vm, vm_global_get(vm, "I"));
    ps_symbol *n1234 = vm_auto_add_integer(vm, 1234);
    ps_symbol *n5678 = vm_auto_add_integer(vm, 5678);
    vm_stack_push(vm, n1234);
    vm_stack_push(vm, n5678);
    ps_symbol_stack_dump(&vm->stack, "3 PUSH?");
    code = vm_exec_op_binary(vm, OP_ADD);
    printf("TEST VM #03 BINARY: 6912; %s %d\n", code == PS_ERROR_ZERO ? "OK" : "KO", code);
    code = vm_exec_assign(vm);
    vm_auto_gc(vm);
    ps_symbol_stack_dump(&vm->stack, "3 POP?");
    ps_symbol_table_dump(&vm->symbols, "I=6912?");
    printf("TEST VM #03 BINARY: I := 1234 + 5678 = 6912; %s %d\n", code == PS_ERROR_ZERO ? "OK" : "KO", code);
    printf("TEST VM #03 BINARY: END\n");
    return 0;
}
