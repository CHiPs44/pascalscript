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

#ifndef PS_SYMBOL_TABLE_DEFAULT_SIZE
#define PS_SYMBOL_TABLE_DEFAULT_SIZE 16
#endif

#define PS_SYMBOL_TABLE_NOT_FOUND UINT16_MAX

    /** @brief up to 65,535 symbols in a table as UINT16_MAX is used for "not found" */
    typedef uint16_t ps_symbol_table_size;

    typedef enum e_ps_symbol_table_error
    {
        PS_SYMBOL_TABLE_ERROR_NONE,
        PS_SYMBOL_TABLE_ERROR_EXISTS,
        PS_SYMBOL_TABLE_ERROR_FULL,
        PS_SYMBOL_TABLE_ERROR_INVALID,
    } __attribute__((__packed__)) ps_symbol_table_error;

    /** @brief Symbol table holding names & their values */
    typedef struct s_ps_symbol_table
    {
        ps_symbol_table_size size; /** @brief max count of symbols */
        ps_symbol_table_size used; /** @brief current count of symbols */
        ps_symbol **symbols;       /** @brief symbols array */
    } __attribute__((__packed__)) ps_symbol_table;

#define PS_SYMBOL_TABLE_SIZEOF sizeof(ps_symbol_table)

    /** @brief Enable/disable trace logging */
    extern bool ps_symbol_table_trace;

    /** @brief Allocate and initialize symbol table, use 0 for default size (PS_SYMBOL_TABLE_DEFAULT_SIZE) */
    ps_symbol_table *ps_symbol_table_alloc(ps_symbol_table_size size);

    /** @brief Free symbol table */
    void *ps_symbol_table_free(ps_symbol_table *table);

    /** @brief How many used symbols? */
    ps_symbol_table_size ps_symbol_table_get_used(ps_symbol_table *table);

    /** @brief How many free symbols? */
    ps_symbol_table_size ps_symbol_table_get_free(ps_symbol_table *table);

    /** @brief Find symbol's index in table by name or return PS_SYMBOL_TABLE_NOT_FOUND */
    ps_symbol_table_size ps_symbol_table_find(ps_symbol_table *table, ps_identifier *name);

    /** @brief Find symbol in table by name */
    ps_symbol *ps_symbol_table_get(ps_symbol_table *table, ps_identifier *name);

    /** @brief Add symbol, returning error if table is full or symbol already exists */
    ps_symbol_table_error ps_symbol_table_add(ps_symbol_table *table, ps_symbol *symbol);

    /** @brief Dump symbol table to stderr */
    void ps_symbol_table_dump(FILE *output, char *title, ps_symbol_table *table);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_TABLE_H */
