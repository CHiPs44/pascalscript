/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "pascalscript.h"
#include "symbol.h"
#include "vm.h"

symbol_t default_globals[] = {
    {"__PS_VERSION__", KIND_CONSTANT, TYPE_INTEGER, sizeof(int), {0}},
    {"MAXINT", KIND_CONSTANT, TYPE_INTEGER, sizeof(int), {2147483647L}},
    //  { "PI", KIND_CONSTANT, TYPE_REAL, sizeof(double), {3.141592653589793} },
};

/**
 * @brief Initialize VM
 */
void vm_init(vm_t *vm)
{
    // VM itself
    vm->program = NULL;
    vm->line = -1;
    vm->column = -1;
    // Symbol table
    symbol_table_init(&vm->globals);
    default_globals[0].value.i = (PS_VERSION_MAJOR << 24) | (PS_VERSION_MINOR << 16) | (PS_VERSION_PATCH << 8) | (PS_VERSION_COUNT & 0xff);
    for (int i = 0; i < sizeof(default_globals) / sizeof(default_globals[0]); i += 1)
    {
        symbol_table_add(&vm->globals, &default_globals[i]);
    }
    symbol_table_add(&vm->globals, &default_globals[0]);
    symbol_table_add(&vm->globals, &default_globals[1]);
    // Stack
    symbol_stack_init(&vm->stack);
}

/**
 * @brief Get global
 *
 * @param VM
 * @param Name normalized
 * @return global or NULL if not found
 */
symbol_t *vm_global_get(vm_t *vm, char *name)
{
    return symbol_table_get(&vm->globals, name);
}

/**
 * @brief Add global
 *
 * @param VM
 * @param Symbol
 * @return Index of added symbol (>=0) or error (<0)
 */
int vm_global_add(vm_t *vm, symbol_t *symbol)
{
    return symbol_table_add(&vm->globals, symbol);
}

/**
 * @brief
 *
 * @param VM
 * @param Symbol name
 * @return index of symbol or -1 if not found
 */
int vm_global_del(vm_t *vm, char *name)
{
    return symbol_table_del(&vm->globals, name);
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
    int index = symbol_table_add(&vm->globals, &symbol);
    return index >= 0 ? &vm->globals.symbols[index] : NULL;
}

int vm_auto_free(vm_t *vm, char *name)
{
    return symbol_table_free(&vm->globals, name);
}

/* EOF */
