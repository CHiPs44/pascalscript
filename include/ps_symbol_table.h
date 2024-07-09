/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
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

#define PS_SYMBOL_TABLE_ERROR_EXISTS (-1)
#define PS_SYMBOL_TABLE_ERROR_FULL (-2)

    /* clang-format off */

    typedef struct _ps_symbol_table
    {
        int         count;
        ps_symbol symbols[PS_SYMBOL_TABLE_SIZE];
    } ps_symbol_table;

    extern void      ps_symbol_table_init(ps_symbol_table *table);
    extern void      ps_symbol_table_dump(ps_symbol_table *table, char *title);
    extern int       ps_symbol_table_find(ps_symbol_table *table, char *name);
    extern ps_symbol *ps_symbol_table_get (ps_symbol_table *table, char *name);
    extern int       ps_symbol_table_add (ps_symbol_table *table, ps_symbol *symbol);
    extern int       ps_symbol_table_delete(ps_symbol_table *table, char *name);
    extern int       ps_symbol_table_free(ps_symbol_table *table, char *name);
    extern int       ps_symbol_table_gc  (ps_symbol_table *table);

    /* clang-format on */

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_TABLE_H */
