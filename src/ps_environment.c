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
    environment->symbols = ps_symbol_table_init(size);
    if (environment->symbols == NULL)
        return ps_environment_done(environment);
    environment->parent = parent;
    memcpy(environment->name, name, PS_IDENTIFIER_SIZE);
    environment->name[PS_IDENTIFIER_SIZE - 1] = '\0'; // Ensure null-termination
    return environment;
}

ps_environment *ps_environment_done(ps_environment *environment)
{
    if (environment->symbols != NULL)
    {
        ps_symbol_table_done(environment->symbols);
        environment->symbols = NULL;
    }
    free(environment);
    return NULL;
}

bool ps_environment_add_symbol(ps_environment *environment, ps_symbol *symbol)
{
    environment->error = ps_symbol_table_add(environment->symbols, symbol);
    return environment->error == PS_SYMBOL_TABLE_ERROR_NONE;
}

ps_symbol *ps_environment_find_symbol(ps_environment *environment, ps_identifier *name)
{
    ps_symbol *symbol;
    do
    {
        symbol = ps_symbol_table_get(environment->symbols, name);
        if (symbol != NULL)
            return symbol;
        environment = environment->parent;
    } while (environment != NULL);
    return NULL;
}
