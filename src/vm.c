/*
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "symbol.h"
#include "vm.h"

symbol_t default_globals[] = {
    {"_PS_VERSION", KIND_CONSTANT, TYPE_INTEGER, sizeof(int), 0x00000001L},  // 0.0.0.1
    {"_PS_VER_DATE", KIND_CONSTANT, TYPE_INTEGER, sizeof(int), 0x20230805L}, // 5-aug-2023
    {"MAXINT", KIND_CONSTANT, TYPE_INTEGER, sizeof(int), 2147483647L},
    //  { "PI", KIND_CONSTANT, TYPE_REAL, sizeof(double), 3.141592653589793 },
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
    for (int i = 0; i < sizeof(default_globals) / sizeof(default_globals[0]); i += 1)
    {
        symbol_table_add(&vm->globals, &default_globals[i]);
    }
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
