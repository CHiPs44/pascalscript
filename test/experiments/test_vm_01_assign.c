/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>
#include <sys/resource.h>

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
    struct rlimit rl = {1024 * 1024 * 8, 1024 * 1024 * 8};
    setrlimit(RLIMIT_AS, &rl);

    int result;
    ps_error code;

    printf("TEST VM #01 ASSIGN: BEGIN\n");
    ps_vm_init_runtime(vm);
    ps_symbol_table_dump(&vm->symbols, "Init", stdout);
    printf("TEST VM #01 ASSIGN: INIT OK\n");

    ps_symbol variable_i = {"I", PS_SYMBOL_KIND_VARIABLE, PS_TYPE_INTEGER, sizeof(ps_integer), {0}};
    result = ps_vm_global_add(vm, &variable_i);
    printf("TEST VM #01 ASSIGN: VAR I: INTEGER; %s %d\n", result == 0 ? "OK" : "KO", result);
    ps_symbol_table_dump(&vm->symbols, "VAR I: INTEGER;", stdout);

    ps_symbol *three = vm_auto_add_integer(vm, 3);
    ps_vm_push(vm, ps_vm_global_get(vm, "I"));
    ps_vm_push(vm, three);
    ps_symbol_stack_dump(&vm->stack, "2 PUSH?");
    code = ps_vm_exec_assign(vm);
    ps_vm_auto_gc(vm);
    ps_symbol_stack_dump(&vm->stack, "2 POP?");
    ps_symbol_table_dump(&vm->symbols, "I=3?", stdout);
    printf("TEST VM #01 ASSIGN: I := 3; %s %d\n", code == PS_ERROR_NONE ? "OK" : "KO", code);

    ps_symbol constant_k = {"K", PS_SYMBOL_KIND_CONSTANT, PS_TYPE_INTEGER, sizeof(ps_integer), {1234}};
    result = ps_vm_global_add(vm, &constant_k);
    printf("TEST VM #01 ASSIGN: CONST K = 1234; %s %d\n", result == 0 ? "OK" : "KO", result);
    ps_symbol_table_dump(&vm->symbols, "CONST K = 1234;", stdout);

    ps_symbol *_5678 = vm_auto_add_integer(vm, 5678);
    ps_vm_push(vm, ps_vm_global_get(vm, "K"));
    ps_vm_push(vm, _5678);
    ps_symbol_stack_dump(&vm->stack, "2 PUSH?");
    code = ps_vm_exec_assign(vm);
    ps_vm_auto_gc(vm);
    ps_symbol_stack_dump(&vm->stack, "2 POP?");
    ps_symbol_table_dump(&vm->symbols, "K=1234?", stdout);
    printf("TEST VM #01 ASSIGN: K := 5678; %s %d\n", code == PS_ERROR_TYPE_MISMATCH ? "OK" : "KO", code);

    printf("TEST VM #01 ASSIGN: END\n");
    return 0;
}
