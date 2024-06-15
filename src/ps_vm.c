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
#include "ps_vm.h"

/**
 * @brief Initialize VM
 */
void vm_init(vm_t *vm)
{
    symbol_t symbol;
    symbol_table_init(&vm->symbols);
    /*******************************************/
    strcpy(symbol.name, "__PS_VERSION__");
    symbol.kind = KIND_CONSTANT;
    symbol.scope = PS_SCOPE_GLOBAL;
    ps_value_unsigned(&symbol.value,
                      (PS_VERSION_MAJOR << 24) | (PS_VERSION_MINOR << 16) | (PS_VERSION_PATCH << 8) | (PS_VERSION_INDEX & 0xff));
    symbol_table_add(&vm->symbols, &symbol);
    /*******************************************/
    strcpy(symbol.name, "__PS_VERTEXT__");
    symbol.kind = KIND_CONSTANT;
    symbol.scope = PS_SCOPE_GLOBAL;
    symbol.value.type = PS_TYPE_STRING;
    symbol.value.size = ps_string_max + 1;
    snprintf(&symbol.value.data.s, ps_string_max, "%d.%d.%d.%d",
             PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_INDEX);
    symbol_table_add(&vm->symbols, &symbol);
    /*******************************************/
    strcpy(symbol.name, "MAXINT");
    symbol.kind = KIND_CONSTANT;
    symbol.scope = PS_SCOPE_GLOBAL;
    ps_value_integer(&symbol.value, PS_INTEGER_MAX);
    symbol_table_add(&vm->symbols, &symbol);
    /*******************************************/
    strcpy(symbol.name, "MAXUINT");
    symbol.kind = KIND_CONSTANT;
    symbol.scope = PS_SCOPE_GLOBAL;
    ps_value_integer(&symbol.value, PS_UNSIGNED_MAX);
    symbol_table_add(&vm->symbols, &symbol);
    /*******************************************/
    strcpy(symbol.name, "FALSE");
    symbol.kind = KIND_CONSTANT;
    symbol.scope = PS_SCOPE_GLOBAL;
    ps_value_boolean(&symbol.value, false);
    symbol_table_add(&vm->symbols, &symbol);
    /*******************************************/
    strcpy(symbol.name, "TRUE");
    symbol.kind = KIND_CONSTANT;
    symbol.scope = PS_SCOPE_GLOBAL;
    ps_value_boolean(&symbol.value, true);
    symbol_table_add(&vm->symbols, &symbol);
    /*******************************************/
    strcpy(symbol.name, "PI");
    symbol.kind = KIND_CONSTANT;
    symbol.scope = PS_SCOPE_GLOBAL;
    ps_value_real(&symbol.value, 3.141592653589793);
    symbol_table_add(&vm->symbols, &symbol);
    /* Stack */
    symbol_stack_init(&vm->stack);
}

/**
 * @brief Get global symbol
 *
 * @param VM
 * @param already normalized name
 * @return global or NULL if not found
 */
symbol_t *vm_global_get(vm_t *vm, char *name)
{
    symbol_t *symbol = symbol_table_get(&vm->symbols, name);
}

/**
 * @brief Add global symbol
 *
 * @param VM
 * @param Symbol
 * @return Index of added symbol (>=0) or error (<0)
 */
int vm_global_add(vm_t *vm, symbol_t *symbol)
{
    return symbol_table_add(&vm->symbols, symbol);
}

/**
 * @brief Delete global symbol
 *
 * @param VM
 * @param Symbol name
 * @return index of symbol or -1 if not found
 */
int vm_global_delete(vm_t *vm, char *name)
{
    return symbol_table_delete(&vm->symbols, name);
}

int vm_stack_push(vm_t *vm, symbol_t *symbol)
{
    return symbol_stack_push(&vm->stack, symbol);
}

symbol_t *vm_stack_pop(vm_t *vm)
{
    return symbol_stack_pop(&vm->stack);
}

symbol_t *vm_auto_add_value(vm_t *vm, ps_value_t *value)
{
    symbol_t symbol;
    strcpy(symbol.name, "");
    symbol.kind = KIND_AUTO;
    symbol.scope = PS_SCOPE_GLOBAL;
    symbol.value.type = PS_TYPE_INTEGER;
    symbol.value.size = sizeof(ps_integer_t);
    symbol.value.data.i = value;
    int index = symbol_table_add(&vm->symbols, &symbol);
    return index >= 0 ? &vm->symbols.symbols[index] : NULL;
}

symbol_t *vm_auto_add_integer(vm_t *vm, ps_integer_t value)
{
    symbol_t symbol;
    strcpy(symbol.name, "");
    symbol.kind = KIND_AUTO;
    symbol.scope = PS_SCOPE_GLOBAL;
    symbol.value.type = PS_TYPE_INTEGER;
    symbol.value.size = sizeof(ps_integer_t);
    symbol.value.data.i = value;
    int index = symbol_table_add(&vm->symbols, &symbol);
    return index >= 0 ? &vm->symbols.symbols[index] : NULL;
}

/**
 * @brief Free auto variable after use
 *
 * @param VM
 * @param string Normalized name
 * @return index of symbol or -1 if not found
 */
int vm_auto_free(vm_t *vm, char *name)
{
    return symbol_table_free(&vm->symbols, name);
}

/**
 * @brief Garbage collector: release free symbols
 *
 * @param VM
 * @return Count of garbage collected symbols
 */
int vm_auto_gc(vm_t *vm)
{
    int count = symbol_table_gc(&vm->symbols);
    fprintf(stderr, "*** VM_AUTO_GC: %d symbol%s freed\n", count, count > 0 ? "s" : "");
    return count;
}

/**
 * @brief Execute ASSIGN statement
 *      1. POP value
 *      2. POP variable
 *      3. SET variable TO value
 */
error_t vm_exec_assign(vm_t *vm)
{
    symbol_t *value = vm_stack_pop(vm);
    if (value == NULL)
        return RUNTIME_ERROR_STACK_EMPTY;
    if (value->kind == KIND_AUTO)
        vm_auto_free(vm, value->name);
    symbol_t *variable = vm_stack_pop(vm);
    if (variable == NULL)
        return RUNTIME_ERROR_STACK_EMPTY;
    if (variable->kind == KIND_CONSTANT)
        return RUNTIME_ERROR_ASSIGN_TO_CONST;
    if (variable->kind != KIND_VARIABLE)
        return RUNTIME_ERROR_EXPECTED_VARIABLE;
    if (variable->value.type != value->value.type)
        return RUNTIME_ERROR_TYPE_MISMATCH;
    variable->value = value->value;
    fprintf(stderr, "*** VM_EXEC_ASSIGN: %s := %d\n", variable->name, variable->value.data.i);
    return RUNTIME_ERROR_NONE;
}

error_t vm_exec_sys(vm_t *vm)
{
    symbol_t *command = vm_stack_pop(vm);
    if (command == NULL)
        return RUNTIME_ERROR_STACK_EMPTY;
    if (command->kind == KIND_AUTO)
        vm_auto_free(vm, command->name);

    return ERROR_NOT_IMPLEMENTED;
}

error_t vm_exec_xxx(vm_t *vm)
{
    return ERROR_NOT_IMPLEMENTED;
}

/* EOF */
