/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_config.h"
#include "ps_error.h"
#include "ps_symbol.h"
#include "ps_value.h"
#include "ps_version.h"
#include "ps_vm.h"

ps_vm *ps_vm_init(ps_vm *vm)
{
    if (vm == NULL)
    {
        vm = calloc(1, sizeof(ps_vm));
        if (vm == NULL)
        {
            return NULL;
        }
        vm->allocated = true;
    }
    else
    {
        vm->allocated = false;
    }
    /* Symbol table */
    vm->symbols = ps_symbol_table_init(NULL);
    if (vm->symbols == NULL)
    {
        if (vm->allocated)
            free(vm);
        return NULL;
    }
    vm->stack = ps_symbol_stack_init(NULL);
    if (vm->stack == NULL)
    {
        if (vm->allocated)
            free(vm);
        return NULL;
    }
    vm->range_check = false;
    return vm;
}

void ps_vm_free(ps_vm *vm)
{
    if (!vm->allocated)
        return;
    ps_symbol_table_free(vm->symbols);
    ps_symbol_stack_free(vm->stack);
    free(vm);
}

bool ps_vm_push(ps_vm *vm, ps_symbol *symbol)
{
    return ps_symbol_stack_push(&vm->stack, symbol);
}

ps_symbol *ps_vm_pop(ps_vm *vm)
{
    return ps_symbol_stack_pop(&vm->stack);
}

/* EOF */
