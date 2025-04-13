/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
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
 * @brief Get global symbol
 *
 * @param VM
 * @param already normalized name
 * @return global or NULL if not found
 */
ps_symbol *ps_vm_global_get(ps_vm *vm, ps_identifier *name)
{
    ps_symbol *symbol = ps_symbol_table_get(vm->symbols, name);
    if (symbol == NULL)
        return NULL;
    if (symbol->scope != PS_SYMBOL_SCOPE_GLOBAL)
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
ps_symbol *ps_vm_global_add(ps_vm *vm, ps_symbol *symbol)
{
    return ps_symbol_table_add(vm->symbols, symbol);
}

/**
 * @brief Delete global symbol
 *
 * @param VM
 * @param Symbol name
 * @return index of symbol or -1 if not found
 */
ps_symbol *ps_vm_global_delete(ps_vm *vm, ps_identifier *name)
{
    return ps_symbol_table_delete(vm->symbols, name);
}

/* EOF */
