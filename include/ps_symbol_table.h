/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_SYMBOL_TABLE_H
#define _PS_SYMBOL_TABLE_H

#include "ps_symbol.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef PS_SYMBOL_TABLE_SIZE
#define PS_SYMBOL_TABLE_SIZE (256)
#endif

#define PS_SYMBOL_TABLE_ERROR_NOT_FOUND (UINT16_MAX - 0)
#define PS_SYMBOL_TABLE_ERROR_EXISTS (UINT16_MAX - 1)
#define PS_SYMBOL_TABLE_ERROR_FULL (UINT16_MAX - 2)

    typedef uint16_t ps_symbol_table_size;

    typedef struct s_ps_symbol_table
    {
        ps_symbol_table_size size;
        ps_symbol_table_size used;
        bool trace : 1;
        bool debug : 1;
        bool allocated : 1;
        uint16_t flags : 13;
        ps_symbol *symbols[PS_SYMBOL_TABLE_SIZE];
    } __attribute__((__packed__)) ps_symbol_table;

#define PS_SYMBOL_TABLE_SIZEOF sizeof(ps_symbol_table)

    /** @brief (Allocate and) initialize symbol table (reset used count & empty all symbols) */
    ps_symbol_table *ps_symbol_table_init(ps_symbol_table *table);

    /** @brief Deallocate symbol table */
    void ps_symbol_table_done(ps_symbol_table *table);

    /** @brief How many used symbols? */
    ps_symbol_table_size ps_symbol_table_used(ps_symbol_table *table);

    /** @brief How many free symbols? */
    ps_symbol_table_size ps_symbol_table_available(ps_symbol_table *table);

    /** @brief Find symbol's index in table by name or PS_SYMBOL_TABLE_ERROR_NOT_FOUND */
    ps_symbol_table_size ps_symbol_table_find(ps_symbol_table *table, ps_identifier *name);

    /** @brief Find symbol in table by name */
    ps_symbol *ps_symbol_table_get(ps_symbol_table *table, ps_identifier *name);

    /** @brief Add symbol, returning NULL if table is full or symbol already exists */
    ps_symbol *ps_symbol_table_add(ps_symbol_table *table, ps_symbol *symbol);

    /** @brief Delete symbol, returning NULL if symbol doesn't exist */
    ps_symbol *ps_symbol_table_delete(ps_symbol_table *table, ps_identifier *name);

    /** @brief Get or add a string constant */
    ps_symbol *ps_symbol_table_add_string_constant(ps_symbol_table *table, char *z);

    /** @brief Dump symbol table to stderr */
    void ps_symbol_table_dump(ps_symbol_table *table, char *title, FILE *output);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_TABLE_H */
