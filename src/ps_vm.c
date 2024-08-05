/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_version.h"
#include "ps_config.h"
#include "ps_error.h"
#include "ps_symbol.h"
#include "ps_value.h"
#include "ps_vm.h"

void ps_runtime_add_system_constant(ps_vm *vm, const char *name, ps_value *value)
{
    ps_symbol symbol;
    strcpy(symbol.name, name);
    symbol.kind = PS_SYMBOL_KIND_CONSTANT;
    symbol.scope = PS_SCOPE_SYSTEM;
    ps_symbol_table_add(&vm->symbols, &symbol);
}

/**
 * @brief Initialize VM
 */
ps_vm *ps_runtime_init()
{
    ps_char buffer[128];
    ps_value *value;

    ps_vm *vm = calloc(1, sizeof(ps_vm));
    if (vm == NULL)
        return NULL;

    /* Symbol table */
    vm->symbols = ps_symbol_table_init();
    if (vm->symbols == NULL)
    {
        free(vm);
        return NULL;
    }
    /* Stack */
    vm->stack = ps_symbol_stack_init();
    if (vm->stack == NULL)
    {
        free(vm->symbols);
        free(vm);
        return NULL;
    }

    // Version
    value = ps_value_set_unsigned(NULL, PS_VERSION_MAJOR);
    ps_runtime_add_system_constant(vm, "PS_VERSION_MAJOR", value);
    value = ps_value_set_unsigned(NULL, PS_VERSION_MINOR);
    ps_runtime_add_system_constant(vm, "PS_VERSION_MINOR", value);
    value = ps_value_set_unsigned(NULL, PS_VERSION_PATCH);
    ps_runtime_add_system_constant(vm, "PS_VERSION_PATCH", value);
    value = ps_value_set_unsigned(NULL, PS_VERSION_INDEX);
    ps_runtime_add_system_constant(vm, "PS_VERSION_INDEX", value);
    snprintf(buffer, sizeof(buffer) - 1, "%d.%d.%d.%d", PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_INDEX);
    value = ps_value_set_string(NULL, buffer, strlen(buffer));
    ps_runtime_add_system_constant(vm, "PS_VERSION", value);
    // Limits
    value = ps_value_set_integer(NULL, ps_integer_max);
    ps_runtime_add_system_constant(vm, "MAXINT", value);
    value = ps_value_set_unsigned(NULL, ps_unsigned_max);
    ps_runtime_add_system_constant(vm, "MAXUINT", value);
    // These are keywords for now
    // value = ps_value_set_boolean(NULL, false);
    // ps_runtime_add_system_constant(vm, "FALSE", value);
    // value = ps_value_set_boolean(NULL, true);
    // ps_runtime_add_system_constant(vm, "TRUE", value);
    // Reals without PI is not conceivable
    value = ps_value_set_real(NULL, 3.141592653589793); // 115997963468544185161590576171875);
    ps_runtime_add_system_constant(vm, "PI", value);
}

/**
 * @brief Get global symbol
 *
 * @param VM
 * @param already normalized name
 * @return global or NULL if not found
 */
ps_symbol *vm_global_get(ps_vm *vm, char *name)
{
    ps_symbol *symbol = ps_symbol_table_get(&vm->symbols, name);
    if (symbol == NULL)
        return NULL;
    if (symbol->kind != PS_SCOPE_GLOBAL)
        return NULL;
    return symbol;
}

/**
 * @brief Add global symbol
 *
 * @param VM
 * @param Symbol
 * @return Index of added symbol (>=0) or error (<0)
 */
int vm_global_add(ps_vm *vm, ps_symbol *symbol)
{
    return ps_symbol_table_add(&vm->symbols, symbol);
}

/**
 * @brief Delete global symbol
 *
 * @param VM
 * @param Symbol name
 * @return index of symbol or -1 if not found
 */
int vm_global_delete(ps_vm *vm, char *name)
{
    return ps_symbol_table_delete(&vm->symbols, name);
}

int vm_stack_push(ps_vm *vm, ps_symbol *symbol)
{
    return ps_symbol_stack_push(&vm->stack, symbol);
}

ps_symbol *vm_stack_pop(ps_vm *vm)
{
    return ps_symbol_stack_pop(&vm->stack);
}

ps_symbol *vm_auto_add_value(ps_vm *vm, ps_value *value)
{
    ps_symbol symbol;
    strcpy(symbol.name, "");
    symbol.kind = PS_SYMBOL_KIND_AUTO;
    symbol.scope = PS_SCOPE_GLOBAL;
    symbol.value.type = PS_TYPE_INTEGER;
    symbol.value.size = sizeof(ps_integer);
    symbol.value.data.i = value;
    int index = ps_symbol_table_add(&vm->symbols, &symbol);
    return index >= 0 ? &vm->symbols.symbols[index] : NULL;
}

ps_symbol *vm_auto_add_value(ps_vm *vm, ps_scope scope, ps_value value)
{
    ps_symbol symbol;
    strcpy(symbol.name, "");
    symbol.kind = PS_SYMBOL_KIND_AUTO;
    symbol.scope = scope;
    symbol.value.type = PS_TYPE_INTEGER;
    symbol.value.size = sizeof(ps_integer);
    symbol.value.data.i = value;
    int index = ps_symbol_table_add(&vm->symbols, &symbol);
    return index >= 0 ? &vm->symbols.symbols[index] : NULL;
}

/**
 * @brief Free auto variable after use
 *
 * @param VM
 * @param string Normalized name
 * @return index of symbol or -1 if not found
 */
int vm_auto_free(ps_vm *vm, char *name)
{
    return ps_symbol_table_free(&vm->symbols, name);
}

/**
 * @brief Garbage collector: release free symbols
 *
 * @param VM
 * @return Count of garbage collected symbols
 */
int vm_auto_gc(ps_vm *vm)
{
    int count = ps_symbol_table_gc(&vm->symbols);
    fprintf(stderr, "*** VM_AUTO_GC: %d symbol%s freed\n", count, count > 0 ? "s" : "");
    return count;
}

/**
 * @brief Execute ASSIGN statement
 *      1. POP value
 *      2. POP variable
 *      3. SET variable TO value
 */
ps_error vm_exec_assign(ps_vm *vm)
{
    ps_symbol *value = vm_stack_pop(vm);
    if (value == NULL)
        return PS_RUNTIME_ERROR_STACK_EMPTY;
    if (value->kind == PS_SYMBOL_KIND_AUTO)
        vm_auto_free(vm, value->name);
    ps_symbol *variable = vm_stack_pop(vm);
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

ps_error vm_exec_sys(ps_vm *vm)
{
    ps_symbol *command = vm_stack_pop(vm);
    if (command == NULL)
        return PS_RUNTIME_ERROR_STACK_EMPTY;
    if (command->kind == PS_SYMBOL_KIND_AUTO)
        vm_auto_free(vm, command->name);

    return PS_ERROR_NOT_IMPLEMENTED;
}

ps_error vm_exec_xxx(ps_vm *vm)
{
    return PS_ERROR_NOT_IMPLEMENTED;
}

/* EOF */
