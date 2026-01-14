/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>
#include <sys/resource.h>

#include "../include/ps_vm.h"
#include "../include/ps_vm_operator.h"

ps_vm _vm;
ps_vm *vm = &_vm;

#include "../src/ps_symbol_table.c"
#include "../src/ps_symbol_stack.c"
#include "../src/ps_vm.c"
#include "../src/ps_vm_operator.c"

int main(void)
{
    struct rlimit rl = {1024 * 1024 * 8, 1024 * 1024 * 8};
    setrlimit(RLIMIT_AS, &rl);

    int result;
    ps_error code;
    printf("TEST VM #02 UNARY: BEGIN\n");
    ps_vm_init_runtime(vm);
    ps_symbol_table_dump(&vm->symbols, "Init", stdout);
    printf("TEST VM #02 UNARY: INIT OK\n");
    ps_symbol symbol = {"I", PS_SYMBOL_KIND_VARIABLE, PS_TYPE_INTEGER, sizeof(ps_integer), {0}};
    result = ps_vm_global_add(vm, &symbol);
    ps_symbol_table_dump(&vm->symbols, "VAR I: INTEGER;", stdout);
    printf("TEST VM #02 UNARY: VAR I: INTEGER; %s %d\n", result == 0 ? "OK" : "KO", result);
    ps_symbol *three = vm_auto_add_integer(vm, 3);
    ps_vm_push(vm, ps_vm_global_get(vm, "I"));
    ps_vm_push(vm, three);
    code = vm_exec_op_unary(vm, OP_NEG);
    printf("TEST VM #02 UNARY: -3; %s %d\n", code == PS_ERROR_NONE ? "OK" : "KO", code);
    ps_symbol_stack_dump(&vm->stack, "2 PUSH?");
    code = ps_vm_exec_assign(vm);
    // int count =
    ps_vm_auto_gc(vm);
    ps_symbol_stack_dump(&vm->stack, "2 POP?");
    ps_symbol_table_dump(&vm->symbols, "I=-3?", stdout);
    printf("TEST VM #02 UNARY: I := -3; %s %d\n", code == PS_ERROR_NONE ? "OK" : "KO", code);
    printf("TEST VM #02 UNARY: END\n");
    return 0;
}
