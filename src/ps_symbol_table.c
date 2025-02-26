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

void ps_symbol_table_reset(ps_symbol_table *table)
{
    for (int i = 0; i < table->size; i++)
    {
        if (table->symbols[i].kind != PS_SYMBOL_KIND_FREE)
            table->symbols[i].kind = PS_SYMBOL_KIND_FREE;
    }
    table->used = 0;
}

ps_symbol_table *ps_symbol_table_init()
{
    ps_symbol_table *table = calloc(1, sizeof(ps_symbol_table));
    if (table == NULL)
        return NULL;
    table->size = sizeof(table->symbols) / sizeof(ps_symbol);
    ps_symbol_table_reset(table);
}

void ps_symbol_table_done(ps_symbol_table *table)
{
    free(table);
}

void ps_symbol_table_dump(ps_symbol_table *table, char *title)
{
    ps_symbol *symbol;
    ps_symbol_table_size free = 0;
    ps_symbol_table_size used = 0;
    fprintf(stderr, "*** Symbol table %s (%d/%d) ***\n", title, table->used, table->size);
    //                        1         2         3         4         5         6         7         8         9        10
    //               1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456
    //                1234 1234567890123456789012345678901 12345678 12345678 12345678 12345678 1234567890123456789012345678901
    fprintf(stderr, "┏━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
    fprintf(stderr, "┃  # ┃            Name               ┃  Kind  ┃ Scope  ┃  Type  ┃              Value            ┃\n");
    fprintf(stderr, "┣━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
    for (int i = 0; i < table->size; i++)
    {
        if (table->symbols[i].kind == PS_SYMBOL_KIND_FREE)
        {
            free += 1;
        }
        else
        {
            used += 1;
            symbol = &table->symbols[i];
            char *kind_name = ps_symbol_get_kind_name(symbol->kind);
            char *scope_name = ps_symbol_get_scope_name(symbol->scope);
            char *type_name = ps_value_get_type_name(symbol->value.type);
            char *buffer = ps_value_get_value(&symbol->value);
            fprintf(stderr, "┃%04d┃%-*s┃%-8s┃%-8s┃%-8s┃%8lu┃%-*s┃\n",
                    i, 31, symbol->name, kind_name, scope_name, type_name, 31, buffer);
        }
    }
    fprintf(stderr, "┗━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━┻━━━━━━━━┻━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
    fprintf(stderr, "(free=%d/used=%d/size=%d => %s)\n", free, used, free + used, free + used == table->size ? "OK" : "KO");
}

ps_symbol_table_size ps_symbol_table_find(ps_symbol_table *table, char *name)
{
    int index = 0;
    // ps_symbol_normalize_name(name);
    while (index < table->size)
    {
        if (table->symbols[index].kind != PS_SYMBOL_KIND_FREE &&
            strcmp(name, table->symbols[index].name) == 0)
        {
            return index;
        }
        index += 1;
    }
    return PS_SYMBOL_TABLE_ERROR_NOT_FOUND;
}

ps_symbol *ps_symbol_table_get(ps_symbol_table *table, char *name)
{
    ps_symbol *symbol = NULL;
    ps_symbol_table_size index = ps_symbol_table_find(table, name);
    if (index >= 0)
    {
        symbol = &table->symbols[index];
    }
    return symbol;
}

ps_symbol_table_size ps_symbol_table_add(ps_symbol_table *table, ps_symbol *symbol)
{
    if (symbol->kind == PS_SYMBOL_KIND_FREE)
        return PS_SYMBOL_TABLE_ERROR_KIND;
    if (table->used >= table->size)
    {
        return PS_SYMBOL_TABLE_ERROR_FULL;
    }
    ps_symbol_table_size index = ps_symbol_table_find(table, symbol->name);
    if (index >= 0)
    {
        return PS_SYMBOL_TABLE_ERROR_EXISTS;
    }
    // Find first free location (table->used *must* be accurate)
    index = 0;
    while (table->symbols[index].kind != PS_SYMBOL_KIND_FREE)
    {
        index += 1;
    }
    if (symbol->kind == PS_SYMBOL_KIND_AUTO)
        snprintf(table->symbols[index].name, PS_IDENTIFIER_MAX, PS_SYMBOL_AUTO_FORMAT, index);
    else
        strncpy(table->symbols[index].name, symbol->name, PS_IDENTIFIER_MAX);
    table->symbols[index].kind = symbol->kind;
    table->symbols[index].value.type = symbol->value.type;
    table->symbols[index].value.data = symbol->value.data;
    // if (symbol->value.type == PS_TYPE_STRING)
    // {
    //     strncpy(table->symbols[index].value.data.s.str,
    //             symbol->value.data.s.str,
    //             ps_string_max);
    // }
    // else
    // {
    // table->symbols[index].value.data = symbol->value.data;
    // }
    table->used += 1;
    return index;
}

/**
 * @brief Delete symbol by name
 *
 * @param Table
 * @param Normalized name
 * @return index of symbol or -1 if not found
 */
ps_symbol_table_size ps_symbol_table_delete(ps_symbol_table *table, char *name)
{
    int index = ps_symbol_table_find(table, name);
    if (index >= 0)
    {
        table->symbols[index].kind = PS_SYMBOL_KIND_FREE;
        table->used -= 1;
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
ps_symbol_table_size ps_symbol_table_free(ps_symbol_table *table, char *name)
{
    int index = ps_symbol_table_find(table, name);
    if (index >= 0)
    {
        table->symbols[index].kind = PS_SYMBOL_KIND_FREE;
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
ps_symbol_table_size ps_symbol_table_gc(ps_symbol_table *table)
{
    int count = 0;
    for (int i = 0; i < table->size; i++)
    {
        if (table->symbols[i].kind == PS_SYMBOL_KIND_FREE)
        {
            table->symbols[i].kind = PS_SYMBOL_KIND_FREE;
            table->used -= 1;
            count += 1;
        }
    }
    return count;
}

/* EOF */
