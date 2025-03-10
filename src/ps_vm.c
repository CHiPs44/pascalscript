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

void ps_runtime_add_system_constant(ps_vm *vm, const char *name, ps_value *value)
{
    ps_symbol symbol;
    strcpy(symbol.name, name);
    symbol.kind = PS_SYMBOL_KIND_CONSTANT;
    symbol.scope = PS_SYMBOL_SCOPE_SYSTEM;
    ps_symbol_table_add(vm->symbols, &symbol);
}

/**
 * @brief Initialize VM
 */
ps_vm *ps_vm_init_runtime(ps_vm *vm)
{
    char buffer[32];
    ps_value *value;
    bool allocated=false;

    if (vm == NULL)
    {
        vm = calloc(1, sizeof(ps_vm));
        if (vm == NULL)
        {
            return NULL;
        }
        allocated=true;
    }

    /* Symbol table */
    vm->symbols = ps_symbol_table_init(NULL);
    if (vm->symbols == NULL)
    {
        if (allocated) free(vm);
        return NULL;
    }
    /* Stack */
    vm->stack = ps_symbol_stack_init();
    if (vm->stack == NULL)
    {
        free(vm->symbols);
        if (allocated) free(vm);
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
    value = ps_value_set_string(NULL, buffer, strlen(buffer), strlen(buffer));
    ps_runtime_add_system_constant(vm, "PS_VERSION", value);
    // Limits
    value = ps_value_set_integer(NULL, ps_integer_max);
    ps_runtime_add_system_constant(vm, "MAXINT", value);
    value = ps_value_set_unsigned(NULL, ps_unsigned_max);
    ps_runtime_add_system_constant(vm, "MAXUINT", value);
    // These are keywords for now (until enums are implemented)
    // value = ps_value_set_boolean(NULL, false);
    // ps_runtime_add_system_constant(vm, "FALSE", value);
    // value = ps_value_set_boolean(NULL, true);
    // ps_runtime_add_system_constant(vm, "TRUE", value);
    // Reals without PI is not conceivable
    value = ps_value_set_real(NULL, 3.141592653589793); // 115997963468544185161590576171875);
    ps_runtime_add_system_constant(vm, "PI", value);
}

int ps_vm_push(ps_vm *vm, ps_symbol *symbol)
{
    return ps_symbol_stack_push(&vm->stack, symbol);
}

ps_symbol *ps_vm_pop(ps_vm *vm)
{
    return ps_symbol_stack_pop(&vm->stack);
}

ps_symbol *ps_vm_auto_add_value(ps_vm *vm, ps_symbol_scope scope, ps_value value)
{
    ps_symbol symbol;
    strcpy(symbol.name, "");
    symbol.kind = PS_SYMBOL_KIND_AUTO;
    symbol.scope = scope;
    symbol.value.type = PS_TYPE_INTEGER;
    // symbol.value.size = sizeof(ps_integer);
    symbol.value.data.i = value.data.i;
    int index = ps_symbol_table_add(vm->symbols, &symbol);
    return index >= 0 ? &vm->symbols->symbols[index] : NULL;
}

/**
 * @brief Free auto variable after use
 *
 * @param VM
 * @param string Normalized name
 * @return index of symbol or -1 if not found
 */
int ps_vm_auto_free(ps_vm *vm, char *name)
{
    return ps_symbol_table_free(vm->symbols, name);
}

/**
 * @brief Garbage collector: release free symbols
 *
 * @param VM
 * @return Count of garbage collected symbols
 */
int ps_vm_auto_gc(ps_vm *vm)
{
    int count = ps_symbol_table_gc(vm->symbols);
    fprintf(stderr, "*** VM_AUTO_GC: %d symbol%s freed\n", count, count > 0 ? "s" : "");
    return count;
}

bool ps_vm_load_source(ps_vm *vm, char *source, size_t length)
{
    bool ok = ps_buffer_set_text(&vm->parser->lexer[0].buffer, source, length);
    return ok;
}

/* EOF */
