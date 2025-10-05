/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_environment.h"
#include "ps_memory.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"

ps_environment *ps_environment_alloc(ps_environment *parent, ps_identifier *name, ps_symbol_table_size size)
{
    ps_environment *environment = ps_memory_malloc(sizeof(ps_environment));
    if (environment == NULL)
        return NULL; // errno = ENOMEM
    environment->symbols = ps_symbol_table_alloc(size);
    if (environment->symbols == NULL)
        return ps_environment_free(environment);
    environment->parent = parent;
    memcpy(environment->name, name, PS_IDENTIFIER_SIZE);
    return environment;
}

ps_environment *ps_environment_free(ps_environment *environment)
{
    if (environment != NULL)
    {
        if (environment->symbols != NULL)
            environment->symbols = ps_symbol_table_free(environment->symbols);
        environment->parent = NULL;
        ps_memory_free(environment);
    }
    return NULL;
}

bool ps_environment_add_symbol(ps_environment *environment, ps_symbol *symbol)
{
    environment->error = ps_symbol_table_add(environment->symbols, symbol);
    return environment->error == PS_SYMBOL_TABLE_ERROR_NONE;
}

ps_symbol *ps_environment_find_symbol(ps_environment *environment, ps_identifier *name, bool local)
{
    ps_symbol *symbol;
    do
    {
        symbol = ps_symbol_table_get(environment->symbols, name);
        if (symbol != NULL)
            return symbol;
        if (local)
            break;
        environment = environment->parent;
    } while (environment != NULL);
    return NULL;
}
