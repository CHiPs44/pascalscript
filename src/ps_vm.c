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

symbol_t default_globals[] = {
    // clang-format off
    {"__PS_VERSION__", KIND_CONSTANT, TYPE_INTEGER         , sizeof(PS_UNSIGNED_INTEGER), {.i=0}},
    {"__PS_VERSTR__" , KIND_CONSTANT, TYPE_STRING          , PS_STRING_MAX +1           , {.s="PascalScript"}},
    {"MAXINT"        , KIND_CONSTANT, TYPE_INTEGER         , sizeof(PS_INTEGER         ), {.i=2147483647L}},
    {"MAXUINT"       , KIND_CONSTANT, TYPE_UNSIGNED_INTEGER, sizeof(PS_UNSIGNED_INTEGER), {.u=0xffffffff}},
    {"PI"            , KIND_CONSTANT, TYPE_REAL            , sizeof(PS_REAL            ), {.r=3.141592653589793}},
    // clang-format on
};

/**
 * @brief Initialize VM
 */
void vm_init(vm_t *vm)
{
    // Program source
    vm->source = NULL;
    vm->length = 0;
    vm->line_count = 0;
    vm->line_starts = NULL;
    vm->line_lengths = NULL;
    vm->current_line = 0;
    vm->current_column = 0;
    vm->current_char = '\0';
    // Symbol table
    symbol_table_init(&vm->symbols);
    default_globals[0].value.i = (PS_VERSION_MAJOR << 24) | (PS_VERSION_MINOR << 16) | (PS_VERSION_PATCH << 8) | (PS_VERSION_INDEX & 0xff);
    for (int i = 0; i < sizeof(default_globals) / sizeof(default_globals[0]); i += 1)
    {
        symbol_table_add(&vm->symbols, &default_globals[i]);
    }
    symbol_table_add(&vm->symbols, &default_globals[0]);
    symbol_table_add(&vm->symbols, &default_globals[1]);
    // Stack
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
    return symbol_table_get(&vm->symbols, name);
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

symbol_t *vm_auto_add_int(vm_t *vm, int value)
{
    symbol_t symbol;
    strcpy(symbol.name, "");
    symbol.kind = KIND_AUTO;
    symbol.type = TYPE_INTEGER;
    symbol.size = sizeof(int);
    symbol.value.i = value;
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
 *      POP value
 *      POP variable
 *      SET variable TO value
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
    if (variable->type != value->type)
        return RUNTIME_ERROR_TYPE_MISMATCH;
    variable->value = value->value;
    fprintf(stderr, "*** VM_EXEC_ASSIGN: %s := %d\n", variable->name, variable->value.i);
    return ERROR_ZERO;
}

/* EOF */
