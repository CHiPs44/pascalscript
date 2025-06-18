/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_environment.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"

ps_environment *ps_environment_init(ps_environment *parent, ps_identifier *name, ps_symbol_table_size size)
{
    ps_environment *environment = calloc(1, sizeof(ps_environment));
    if (environment == NULL)
        return NULL; // errno = ENOMEM
    environment->parent = parent;
    // environment->type = type;
    memcpy(environment->name, name, PS_IDENTIFIER_SIZE);
    environment->symbols = ps_symbol_table_init(size);
    if (environment->symbols == NULL)
    {
        ps_environment_done(environment);
        return NULL; // errno = ENOMEM
    }
    return environment;
}

void ps_environment_done(ps_environment *environment)
{
    if (environment == NULL)
        return;
    if (environment->symbols != NULL)
    {
        ps_symbol_table_done(environment->symbols);
    }
    free(environment);
}

bool ps_environment_add_symbol(ps_environment *environment, ps_symbol *symbol)
{
    // TODO store error in environment
    ps_symbol_table_error error = ps_symbol_table_add(environment->symbols, symbol);
    return error != PS_SYMBOL_TABLE_ERROR_NONE;
}

ps_symbol *ps_environment_find_symbol(ps_environment *environment, ps_identifier *name)
{
    // Search for the symbol in the current environment
    ps_symbol *symbol = ps_symbol_table_get(environment->symbols, name);
    if (symbol != NULL)
        return symbol;
    // If not found, check the parent environment recursively
    // TODO remove recursion
    if (environment->parent != NULL)
        return ps_environment_find_symbol(environment->parent, name);
    // Symbol not found in this environment or any parent
    return NULL;
}
