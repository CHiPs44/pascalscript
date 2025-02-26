/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_SYMBOL_TABLE_H
#define _PS_SYMBOL_TABLE_H

#include "ps_symbol.h"

#ifdef __cplusplus
"C"
{
#endif

#ifndef PS_SYMBOL_TABLE_SIZE
#define PS_SYMBOL_TABLE_SIZE (256)
#endif

#define PS_SYMBOL_AUTO_FORMAT "#AUTO_%04x"

#define PS_SYMBOL_TABLE_ERROR_NOT_FOUND (UINT16_MAX - 1)
#define PS_SYMBOL_TABLE_ERROR_EXISTS (UINT16_MAX - 2)
#define PS_SYMBOL_TABLE_ERROR_FULL (UINT16_MAX - 3)
#define PS_SYMBOL_TABLE_ERROR_KIND (UINT16_MAX - 4)
#define PS_SYMBOL_TABLE_ERROR_TODO (UINT16_MAX - 5)

    typedef uint16_t ps_symbol_table_size;

    /* clang-format off */
    typedef struct s_ps_symbol_table
    {
        ps_symbol_table_size size;
        ps_symbol_table_size used;
        ps_symbol            symbols[PS_SYMBOL_TABLE_SIZE];
    } ps_symbol_table;
    /* clang-format on */

    /** @brief Allocate and initialize symbol table (reset count & mark all symbols as free) */
    ps_symbol_table *ps_symbol_table_init();

    /** @brief Deallocate symbol table */
    void ps_symbol_table_done(ps_symbol_table * table);

    /** @brief Dump symbol table to stderr */
    void ps_symbol_table_dump(ps_symbol_table * table, char *title);

    /** @brief Find symbol's index in table by name */
    ps_symbol_table_size ps_symbol_table_find(ps_symbol_table * table, char *name);

    /** @brief Find symbol in table by name */
    ps_symbol *ps_symbol_table_get(ps_symbol_table * table, char *name);

    /**
     * @brief Add symbol, returning an error if table is full or symbol already exists
     * @return Index of added symbol (>=0) or error (<0)
     */
    ps_symbol_table_size ps_symbol_table_add(ps_symbol_table * table, ps_symbol * symbol);

    ps_symbol_table_size ps_symbol_table_delete(ps_symbol_table * table, char *name);

    ps_symbol_table_size ps_symbol_table_free(ps_symbol_table * table, char *name);

    ps_symbol_table_size ps_symbol_table_gc(ps_symbol_table * table);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_TABLE_H */
