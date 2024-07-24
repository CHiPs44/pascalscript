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
#include "ps_vm.h"

/**
 * @brief Initialize VM
 */
void vm_init(ps_vm *vm)
{
    ps_symbol symbol;
    ps_symbol_table_init(&vm->symbols);
    /*******************************************/
    strcpy(symbol.name, "__PS_VERSION__");
    symbol.kind = SYMBOL_TYPE_CONSTANT;
    symbol.scope = PS_SCOPE_GLOBAL;
    ps_value_set_unsigned(&symbol.value,
                      (PS_VERSION_MAJOR << 24) | (PS_VERSION_MINOR << 16) | (PS_VERSION_PATCH << 8) | (PS_VERSION_INDEX & 0xff));
    ps_symbol_table_add(&vm->symbols, &symbol);
    /*******************************************/
    strcpy(symbol.name, "__PS_VERTEXT__");
    symbol.kind = SYMBOL_TYPE_CONSTANT;
    symbol.scope = PS_SCOPE_GLOBAL;
    symbol.value.type = PS_TYPE_STRING;
    symbol.value.size = ps_string_max + 1;
    snprintf(&symbol.value.data.s.str, ps_string_max, "%d.%d.%d.%d",
             PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_INDEX);
    ps_symbol_table_add(&vm->symbols, &symbol);
    /*******************************************/
    strcpy(symbol.name, "MAXINT");
    symbol.kind = SYMBOL_TYPE_CONSTANT;
    symbol.scope = PS_SCOPE_GLOBAL;
    ps_value_set_integer(&symbol.value, PS_INTEGER_MAX);
    ps_symbol_table_add(&vm->symbols, &symbol);
    /*******************************************/
    strcpy(symbol.name, "MAXUINT");
    symbol.kind = SYMBOL_TYPE_CONSTANT;
    symbol.scope = PS_SCOPE_GLOBAL;
    ps_value_set_unsigned(&symbol.value, PS_UNSIGNED_MAX);
    ps_symbol_table_add(&vm->symbols, &symbol);
    /*******************************************/
    strcpy(symbol.name, "FALSE");
    symbol.kind = SYMBOL_TYPE_CONSTANT;
    symbol.scope = PS_SCOPE_GLOBAL;
    ps_value_set_boolean(&symbol.value, false);
    ps_symbol_table_add(&vm->symbols, &symbol);
    /*******************************************/
    strcpy(symbol.name, "TRUE");
    symbol.kind = SYMBOL_TYPE_CONSTANT;
    symbol.scope = PS_SCOPE_GLOBAL;
    ps_value_set_boolean(&symbol.value, true);
    ps_symbol_table_add(&vm->symbols, &symbol);
    /*******************************************/
    strcpy(symbol.name, "PI");
    symbol.kind = SYMBOL_TYPE_CONSTANT;
    symbol.scope = PS_SCOPE_GLOBAL;
    ps_value_set_real(&symbol.value, 3.141592653589793);
    ps_symbol_table_add(&vm->symbols, &symbol);
    /* Stack */
    ps_symbol_stack_init(&vm->stack);
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
    symbol.kind = SYMBOL_TYPE_AUTO;
    symbol.scope = PS_SCOPE_GLOBAL;
    symbol.value.type = PS_TYPE_INTEGER;
    symbol.value.size = sizeof(ps_integer);
    symbol.value.data.i = value;
    int index = ps_symbol_table_add(&vm->symbols, &symbol);
    return index >= 0 ? &vm->symbols.symbols[index] : NULL;
}

ps_symbol *vm_auto_add_value(ps_vm *vm, ps_value value)
{
    ps_symbol symbol;
    strcpy(symbol.name, "");
    symbol.kind = SYMBOL_TYPE_AUTO;
    symbol.scope = PS_SCOPE_GLOBAL;
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
    if (value->kind == SYMBOL_TYPE_AUTO)
        vm_auto_free(vm, value->name);
    ps_symbol *variable = vm_stack_pop(vm);
    if (variable == NULL)
        return PS_RUNTIME_ERROR_STACK_EMPTY;
    if (variable->kind == SYMBOL_TYPE_CONSTANT)
        return PS_RUNTIME_ERROR_ASSIGN_TO_CONST;
    if (variable->kind != SYMBOL_TYPE_VARIABLE)
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
    if (command->kind == SYMBOL_TYPE_AUTO)
        vm_auto_free(vm, command->name);

    return PS_ERROR_NOT_IMPLEMENTED;
}

ps_error vm_exec_xxx(ps_vm *vm)
{
    return PS_ERROR_NOT_IMPLEMENTED;
}

/* EOF */
