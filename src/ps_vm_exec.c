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

/**
 * @brief Execute ASSIGN statement
 *      1. POP value
 *      2. POP variable
 *      3. SET variable TO value
 */
ps_error ps_vm_exec_assign(ps_vm *vm)
{
    ps_symbol *value = ps_vm_stack_pop(vm);
    if (value == NULL)
        return PS_RUNTIME_ERROR_STACK_EMPTY;
    if (value->kind == PS_SYMBOL_KIND_AUTO)
        ps_vm_auto_free(vm, value->name);
    ps_symbol *variable = ps_vm_stack_pop(vm);
    if (variable == NULL)
        return PS_RUNTIME_ERROR_STACK_EMPTY;
    if (variable->kind == PS_SYMBOL_KIND_CONSTANT)
        return PS_RUNTIME_ERROR_ASSIGN_TO_CONST;
    if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
        return PS_RUNTIME_ERROR_EXPECTED_VARIABLE;
    if (variable->value.type != value->value.type)
        return PS_RUNTIME_ERROR_TYPE_MISMATCH;
    variable->value = value->value;
    fprintf(stderr, "*** VM_EXEC_ASSIGN: %s := %d\n", variable->name, variable->value.data.i);
    return PS_RUNTIME_ERROR_NONE;
}

ps_error ps_vm_exec_sys(ps_vm *vm)
{
    ps_symbol *command = ps_vm_stack_pop(vm);
    if (command == NULL)
        return PS_RUNTIME_ERROR_STACK_EMPTY;
    if (command->kind == PS_SYMBOL_KIND_AUTO)
        ps_vm_auto_free(vm, command->name);

    return PS_ERROR_NOT_IMPLEMENTED;
}

ps_error ps_vm_exec_xxx(ps_vm *vm)
{
    return PS_ERROR_NOT_IMPLEMENTED;
}

/* EOF */
