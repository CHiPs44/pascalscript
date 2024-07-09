/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>

#include "../src/vm.h"
#include "../src/operator.h"

ps_vm _vm;
ps_vm *vm = &_vm;

#include "../src/symbol_table.c"
#include "../src/symbol_stack.c"
#include "../src/vm.c"
#include "../src/operator.c"

int main(void)
{
    int result;
    ps_error_t code;
    printf("TEST VM #02 UNARY: BEGIN\n");
    vm_init(vm);
    ps_symbol_table_dump(&vm->symbols, "Init");
    printf("TEST VM #02 UNARY: INIT OK\n");
    ps_symbol symbol = {"I", KIND_VARIABLE, PS_TYPE_INTEGER, sizeof(ps_integer), {0}};
    result = vm_global_add(vm, &symbol);
    ps_symbol_table_dump(&vm->symbols, "VAR I: INTEGER;");
    printf("TEST VM #02 UNARY: VAR I: INTEGER; %s %d\n", result == 0 ? "OK" : "KO", result);
    ps_symbol *three = vm_auto_add_integer(vm, 3);
    vm_stack_push(vm, vm_global_get(vm, "I"));
    vm_stack_push(vm, three);
    code = vm_exec_op_unary(vm, OP_NEG);
    printf("TEST VM #02 UNARY: -3; %s %d\n", code == PS_ERROR_ZERO ? "OK" : "KO", code);
    ps_symbol_stack_dump(&vm->stack, "2 PUSH?");
    code = vm_exec_assign(vm);
    // int count = 
    vm_auto_gc(vm);
    ps_symbol_stack_dump(&vm->stack, "2 POP?");
    ps_symbol_table_dump(&vm->symbols, "I=-3?");
    printf("TEST VM #02 UNARY: I := -3; %s %d\n", code == PS_ERROR_ZERO ? "OK" : "KO", code);
    printf("TEST VM #02 UNARY: END\n");
    return 0;
}
