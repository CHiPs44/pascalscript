/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>

#include "ps_environment.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"

ps_environment *ps_environment_init(ps_environment *parent, ps_environment_type type, char *name,
                                    ps_symbol_table_size size)
{
    ps_environment *environment = calloc(1, sizeof(ps_environment));
    if (environment == NULL)
        return NULL; // errno = ENOMEM
    environment->parent = parent;
    environment->type = type;
    snprintf(environment->name, PS_IDENTIFIER_SIZE - 1, "%s", name);
    environment->symbols = ps_symbol_table_init(size);
    if (environment->symbols == NULL)
    {
        free(environment);
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
    return ps_symbol_table_add(&environment->symbols, symbol);
}

ps_symbol *ps_environment_find_symbol(ps_environment *environment, ps_identifier *name)
{
    // Search for the symbol in the current environment
    ps_symbol *symbol = ps_symbol_table_find(&environment->symbols, name);
    if (symbol != NULL)
        return symbol;
    // If not found, check the parent environment recursively
    if (environment->parent != NULL)
        return ps_environment_find_symbol(environment->parent, name);

    // Symbol not found in this environment or any parent
    return NULL;
}
