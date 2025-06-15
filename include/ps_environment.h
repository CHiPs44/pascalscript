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

    typedef enum e_ps_environment_type
    {
        PS_ENVIRONMENT_TYPE_SYSTEM = 0,
        PS_ENVIRONMENT_TYPE_PROGRAM,
        PS_ENVIRONMENT_TYPE_UNIT,
        PS_ENVIRONMENT_TYPE_CALLABLE, // function or procedure (or method if we support OOP)
    } ps_environment_type;

    typedef struct s_ps_environment
    {
        struct s_ps_environment *parent; // parent environment (NULL for system environment)
        ps_environment_type type;
        ps_identifier name;
        ps_symbol_table *symbols;
    } ps_environment;

#define PS_ENVIRONMENT_SIZE sizeof(ps_environment)

    /** @brief Initialize environment */
    ps_environment *ps_environment_init(ps_environment *parent, ps_environment_type type, char *name,
                                        ps_symbol_table_size size);

    /** @brief Free environment */
    void ps_environment_done(ps_environment *environment);

    /** @brief Add symbol to environment */
    ps_symbol *ps_environment_add_symbol(ps_environment *environment, ps_symbol *symbol);

    /** @brief Find symbol in environment */
    ps_symbol *ps_environment_find_symbol(ps_environment *environment, ps_identifier *name);

#ifdef __cplusplus
}
#endif

#endif /* _PS_ENVIRONMENT_H */
