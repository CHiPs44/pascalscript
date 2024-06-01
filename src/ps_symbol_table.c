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
void symbol_table_init(symbol_table_t *table)
{
    table->count = 0;
    for (int i = 0; i < SYMBOL_TABLE_SIZE; i++)
    {
        table->symbols[i].kind = KIND_FREE;
    }
}

void symbol_table_dump(symbol_table_t *table, char *title)
{
    symbol_t *symbol;
    fprintf(stderr, "*** Symbol table %s (%d) ***\n", title, table->count);
    fprintf(stderr, "┏━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
    fprintf(stderr, "┃ # ┃Name                           ┃Kind    ┃Scope   ┃Type    ┃Size    ┃Value                          ┃\n");
    fprintf(stderr, "┣━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
    for (int i = 0; i < SYMBOL_TABLE_SIZE; i++)
    {
        if (table->symbols[i].kind != KIND_FREE)
        {
            symbol = &table->symbols[i];
            char *kind_name = symbol_get_kind_name(symbol->kind);
            char *scope_name = symbol_get_scope_name(symbol->scope);
            char *type_name = value_get_type_name(symbol->value.type);
            char *buffer = value_get_value(&symbol->value);
            fprintf(stderr, "┃%03d┃%-*s┃%-8s┃%-8s┃%-8s┃%8lu┃%-*s┃\n",
                    i, MAX_SYMBOL_NAME, symbol->name, kind_name, scope_name, type_name, symbol->value.size, MAX_SYMBOL_NAME, buffer);
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
    while (table->symbols[index].kind != KIND_FREE)
    {
        index += 1;
    }
    if (symbol->kind == KIND_AUTO) // strlen(symbol->name) == 0)
    {
        snprintf(table->symbols[index].name,
                 MAX_SYMBOL_NAME,
                 "#PS_AUTO_%04x",
                 index);
    }
    else
    {
        strncpy(table->symbols[index].name, symbol->name, MAX_SYMBOL_NAME);
    }
    table->symbols[index].kind = symbol->kind;
    table->symbols[index].value.type = symbol->value.type;
    table->symbols[index].value.size = symbol->value.size;
    table->symbols[index].value.data = symbol->value.data;
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
int symbol_table_delete(symbol_table_t *table, char *name)
{
    int index = symbol_table_find(table, name);
    if (index >= 0)
    {
        table->symbols[index].kind = KIND_FREE;
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
        if (table->symbols[i].kind == KIND_FREE)
        {
            table->symbols[i].kind = KIND_FREE;
            table->count -= 1;
            count += 1;
        }
    }
    return count;
}

/* EOF */
