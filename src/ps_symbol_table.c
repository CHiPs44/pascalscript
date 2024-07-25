/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_value.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"

/**
 * @brief Initialize symbol table:
 *          - reset count
 *          - mark all symbols as free
 *
 * @param table
 */
void ps_symbol_table_init(ps_symbol_table *table)
{
    table->count = 0;
    for (int i = 0; i < PS_SYMBOL_TABLE_SIZE; i++)
    {
        table->symbols[i].kind = PS_SYMBOL_TYPE_FREE;
    }
}

void ps_symbol_table_dump(ps_symbol_table *table, char *title)
{
    ps_symbol *symbol;
    fprintf(stderr, "*** Symbol table %s (%d) ***\n", title, table->count);
    fprintf(stderr, "┏━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
    fprintf(stderr, "┃ # ┃Name                           ┃Kind    ┃Scope   ┃Type    ┃Size    ┃Value                          ┃\n");
    fprintf(stderr, "┣━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
    for (int i = 0; i < PS_SYMBOL_TABLE_SIZE; i++)
    {
        if (table->symbols[i].kind != PS_SYMBOL_TYPE_FREE)
        {
            symbol = &table->symbols[i];
            char *kind_name = ps_symbol_get_type_name(symbol->kind);
            char *scope_name = symbol_get_scope_name(symbol->scope);
            char *type_name = ps_value_get_type_name(symbol->value.type);
            char *buffer = ps_value_get_value(&symbol->value);
            fprintf(stderr, "┃%03d┃%-*s┃%-8s┃%-8s┃%-8s┃%8lu┃%-*s┃\n",
                    i, PS_IDENTIFIER_MAX, symbol->name, kind_name, scope_name, type_name, symbol->value.size, PS_IDENTIFIER_MAX, buffer);
        }
    }
    fprintf(stderr, "┗━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━┻━━━━━━━━┻━━━━━━━━┻━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
}

/**
 * @brief Search symbol
 *
 * @param name normalized
 * @return int index of symbol or -1 if not found
 */
int ps_symbol_table_find(ps_symbol_table *table, char *name)
{
    int index = 0;
    // ps_symbol_normalize_name(name);
    while (index < table->count)
    {
        if (strcmp(name, table->symbols[index].name) == 0)
        {
            return index;
        }
        index += 1;
    }
    return -1;
}

/**
 * @brief Get symbol by name
 *
 * @param name normalized
 * @return symbol or NULL if not found
 */
ps_symbol *ps_symbol_table_get(ps_symbol_table *table, char *name)
{
    ps_symbol *symbol = NULL;
    int index = ps_symbol_table_find(table, name);
    if (index >= 0)
    {
        symbol = &table->symbols[index];
    }
    return symbol;
}

/**
 * @brief Add symbol, returning an error
 *        if table is full
 *        or symbol already exists
 *
 * @param Table
 * @param Symbol
 * @return Index of added symbol (>=0) or error (<0)
 */
int ps_symbol_table_add(ps_symbol_table *table, ps_symbol *symbol)
{
    if (table->count >= PS_SYMBOL_TABLE_SIZE)
    {
        return PS_SYMBOL_TABLE_ERROR_FULL;
    }
    int index = ps_symbol_table_find(table, symbol->name);
    if (index >= 0)
    {
        return PS_SYMBOL_TABLE_ERROR_EXISTS;
    }
    // Find first free location
    index = 0;
    while (table->symbols[index].kind != PS_SYMBOL_TYPE_FREE)
    {
        index += 1;
    }
    if (symbol->kind == PS_SYMBOL_TYPE_AUTO)
    {
        snprintf(table->symbols[index].name,
                 PS_IDENTIFIER_MAX,
                 "#PS_AUTO_%04x",
                 index);
    }
    else
    {
        strncpy(table->symbols[index].name, symbol->name, PS_IDENTIFIER_MAX);
    }
    table->symbols[index].kind = symbol->kind;
    table->symbols[index].value.type = symbol->value.type;
    table->symbols[index].value.size = symbol->value.size;
    if (symbol->value.type == PS_TYPE_STRING)
    {
        strncpy(table->symbols[index].value.data.s.str,
                symbol->value.data.s.str,
                ps_string_max);
    }
    else
    {
        table->symbols[index].value.data = symbol->value.data;
    }
    table->count += 1;
    return index;
}

/**
 * @brief Delete symbol by name
 *
 * @param Table
 * @param Normalized name
 * @return index of symbol or -1 if not found
 */
int ps_symbol_table_delete(ps_symbol_table *table, char *name)
{
    int index = ps_symbol_table_find(table, name);
    if (index >= 0)
    {
        table->symbols[index].kind = PS_SYMBOL_TYPE_FREE;
        table->count -= 1;
    }
    return index;
}

/**
 * @brief Free symbol by name, used to clean auto vars after use
 *
 * @param Table
 * @param Normalized name
 * @return index of symbol or -1 if not found
 */
int ps_symbol_table_free(ps_symbol_table *table, char *name)
{
    int index = ps_symbol_table_find(table, name);
    if (index >= 0)
    {
        table->symbols[index].kind = PS_SYMBOL_TYPE_FREE;
    }
    return index;
}

/**
 * @brief Garbage collect:
 *          change state of free symbols to unknown
 *          update table count
 *
 * @param Table
 * @return Count of garbage collected symbols
 */
int ps_symbol_table_gc(ps_symbol_table *table)
{
    int count = 0;
    for (int i = 0; i < PS_SYMBOL_TABLE_SIZE; i++)
    {
        if (table->symbols[i].kind == PS_SYMBOL_TYPE_FREE)
        {
            table->symbols[i].kind = PS_SYMBOL_TYPE_FREE;
            table->count -= 1;
            count += 1;
        }
    }
    return count;
}

/* EOF */
