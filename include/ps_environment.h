/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_ENVIRONMENT_H
#define _PS_ENVIRONMENT_H

#include "ps_symbol.h"
#include "ps_symbol_table.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief For now, just a name, a symbol table and a parent for nested scopes */
    typedef struct s_ps_environment
    {
        struct s_ps_environment *parent; // NULL for system environment
        ps_symbol_table_error error;
        ps_identifier name;
        ps_symbol_table *symbols;
    } ps_environment;

#define PS_ENVIRONMENT_SIZE sizeof(ps_environment)

    /** @brief Initialize environment */
    /** @return NULL if no free memory (errno = ENOMEM) or the environment */
    ps_environment *ps_environment_init(ps_environment *parent, ps_identifier *name, ps_symbol_table_size size);

    /** @brief Free environment */
    ps_environment *ps_environment_done(ps_environment *environment);

    /** @brief Add symbol to environment */
    /** @return true if OK, false otherwise */
    bool ps_environment_add_symbol(ps_environment *environment, ps_symbol *symbol);

    /** @brief Find symbol in environment */
    /** @return NULL if not found */
    ps_symbol *ps_environment_find_symbol(ps_environment *environment, ps_identifier *name);

#ifdef __cplusplus
}
#endif

#endif /* _PS_ENVIRONMENT_H */
