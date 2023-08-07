/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "symbol.h"
#include "symbol_table.h"

/**
 * @brief Initialize symbol table:
 *          - reset count
 *          - mark all symbols as unknown
 * 
 * @param table 
 */
void symbol_table_init(symbol_table_t *table)
{
    table->count = 0;
    for (int i = 0; i < SYMBOL_TABLE_SIZE; i++)
    {
        table->symbols[i].kind = KIND_UNKNOWN;
    }
}

void symbol_table_dump(symbol_table_t *table, char *title)
{
    symbol_t *s;
    fprintf(stderr, "*** Symbol table %s (%d) ***\n", title, table->count);
    fprintf(stderr, "#   name                            kind type value        value\n");
    fprintf(stderr, "--- ------------------------------- ---- ---- ------------ --------\n");
    for (int i = 0; i < SYMBOL_TABLE_SIZE; i++)
    {
        if (table->symbols[i].kind != KIND_UNKNOWN)
        {
            s = &table->symbols[i];
            fprintf(stderr, "%03d %-*s %4d %4d %12d %08x\n",
                    i, MAX_SYMBOL_NAME, s->name, s->kind, s->type, s->value.i, s->value.i);
        }
    }
}

/**
 * @brief Search symbol
 *
 * @param name normalized
 * @return int index of symbol or -1 if not found
 */
int symbol_table_find(symbol_table_t *table, char *name)
{
    int index = 0;
    // symbol_normalize_name(name);
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
symbol_t *symbol_table_get(symbol_table_t *table, char *name)
{
    symbol_t *symbol = NULL;
    int index = symbol_table_find(table, name);
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
int symbol_table_add(symbol_table_t *table, symbol_t *symbol)
{
    if (table->count >= SYMBOL_TABLE_SIZE)
    {
        return SYMBOL_TABLE_ERROR_FULL;
    }
    int index = symbol_table_find(table, symbol->name);
    if (index >= 0)
    {
        return SYMBOL_TABLE_ERROR_EXISTS;
    }
    // Find first free location
    index = 0;
    while (table->symbols[index].kind != KIND_UNKNOWN)
    {
        index += 1;
    }
    if (symbol->kind == KIND_AUTO) // strlen(symbol->name) == 0)
    {
        snprintf(table->symbols[index].name,
                 MAX_SYMBOL_NAME,
                 "_PS_AUTO_%06x_",
                 index);
    }
    else
    {
        strncpy(table->symbols[index].name, symbol->name, MAX_SYMBOL_NAME);
    }
    table->symbols[index].kind = symbol->kind;
    table->symbols[index].type = symbol->type;
    table->symbols[index].size = symbol->size;
    table->symbols[index].value = symbol->value;
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
int symbol_table_del(symbol_table_t *table, char *name)
{
    int index = symbol_table_find(table, name);
    if (index >= 0)
    {
        table->symbols[index].kind = KIND_UNKNOWN;
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
int symbol_table_free(symbol_table_t *table, char *name)
{
    int index = symbol_table_find(table, name);
    if (index >= 0)
    {
        table->symbols[index].kind = KIND_FREE;
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
int symbol_table_gc(symbol_table_t *table)
{
    int count = 0;
    for (int i = 0; i < SYMBOL_TABLE_SIZE; i++)
    {
        if (table->symbols[i].kind = KIND_FREE)
        {
            table->symbols[i].kind = KIND_UNKNOWN;
            table->count -= 1;
            count += 1;
        }
    }
    return count;
}

/* EOF */
