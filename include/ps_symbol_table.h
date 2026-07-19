/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_SYMBOL_TABLE_H
#define _PS_SYMBOL_TABLE_H

#include <stdint.h>

#include "ps_logger.h"
#include "ps_symbol.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef PS_SYMBOL_TABLE_SIZE
#define PS_SYMBOL_TABLE_SIZE 16
#endif

#ifndef PS_SYMBOL_BUCKET_SIZE
#define PS_SYMBOL_BUCKET_SIZE 16
#endif

#define PS_SYMBOL_TABLE_NOT_FOUND UINT16_MAX

    typedef struct s_ps_bucket
    {
        ssize_t size;         /** @brief capacity of the bucket          */
        ssize_t used;         /** @brief number of symbols in the bucket */
        ssize_t more;         /** @brief how much more symbols to grow   */
        ps_symbol *symbols[]; /** @brief array of symbols in the bucket  */
    } ps_bucket;

    /** @brief Symbol table holding names & their values */
    typedef struct s_ps_symbol_table
    {
        ssize_t table_size;   /** @brief count of buckets in table  */
        ssize_t bucket_size;  /** @brief count of symbols in bucket */
        ssize_t used;         /** @brief count of used buckets      */
        ssize_t vars;         /** @brief count of variable symbols  */
        ps_bucket *buckets[]; /** @brief symbol buckets array       */
    } __attribute__((__packed__)) ps_symbol_table;

#define PS_SYMBOL_TABLE_SIZEOF sizeof(ps_symbol_table)

    /** @brief Enable/disable trace logging */
    extern ps_debug_level ps_symbol_table_debug_level;

    /** @brief Allocate and initialize symbol table, use 0 for defaults */
    ps_symbol_table *ps_symbol_table_alloc(ssize_t table_size, ssize_t bucket_size);

    /** @brief Free symbol table */
    void *ps_symbol_table_free(ps_symbol_table *table);

    /** @brief How many used symbols? */
    ssize_t ps_symbol_table_get_used(const ps_symbol_table *table);

    /** @brief How many free symbols? */
    ssize_t ps_symbol_table_get_free(const ps_symbol_table *table);

    /** @brief Find symbol's index in table by name or return PS_SYMBOL_TABLE_NOT_FOUND */
    ssize_t ps_symbol_table_find(const ps_symbol_table *table, const char *name);

    /** @brief Find symbol in table by name */
    ps_symbol *ps_symbol_table_get(const ps_symbol_table *table, const char *name);

    /** @brief Add symbol, returning error if table is full or symbol already exists */
    ps_error ps_symbol_table_add(ps_symbol_table *table, ps_symbol *symbol);

    /** @brief Dump symbol table to stderr */
    void ps_symbol_table_dump(FILE *output, char *title, const ps_symbol_table *table);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_TABLE_H */
