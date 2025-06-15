/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_string.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_value.h"

void ps_symbol_table_reset(ps_symbol_table *table, bool free_symbols)
{
    for (int i = 0; i < table->size; i++)
    {
        if (table->symbols[i] != NULL)
        {
            if (free_symbols && table->symbols[i]->allocated)
            {
                ps_symbol_free(table->symbols[i]);
            }
            table->symbols[i] = NULL;
        }
    }
    // table->error = PS_SYMBOL_TABLE_ERROR_NONE;
    table->used = 0;
}

ps_symbol_table *ps_symbol_table_init(ps_symbol_table_size size)
{
    ps_symbol_table *table;
    table = calloc(1, sizeof(ps_symbol_table));
    if (table == NULL)
        return NULL;
    table->debug = false;
    table->trace = false;
    table->size = size > 0 ? size : PS_SYMBOL_TABLE_DEFAULT_SIZE;
    table->symbols = calloc(table->size, sizeof(ps_symbol *));
    if (table->symbols == NULL)
    {
        free(table);
        return NULL; // errno = ENOMEM
    }
    ps_symbol_table_reset(table, false);
    return table;
}

void ps_symbol_table_done(ps_symbol_table *table)
{
    if (table == NULL)
        return;
    if (table->symbols != NULL)
        free(table->symbols);
    free(table);
}

ps_symbol_table_size ps_symbol_table_get_used(ps_symbol_table *table)
{
    if (table == NULL)
        return 0;
    return table->used;
}

ps_symbol_table_size ps_symbol_table_get_free(ps_symbol_table *table)
{
    if (table == NULL)
        return 0;
    return table->size - table->used;
}

ps_symbol_hash_key ps_symbol_get_hash_key(char *name)
{
    // DJB2, cf. https://en.wikipedia.org/wiki/Universal_hashing#Hashing_strings
    // 33 * x => 32 * x + x => x << 5 + x
    ps_symbol_hash_key hash = 5381u;
    while (*name)
        hash = (hash << 5) + hash + *name++;
    return hash;
}

ps_symbol_table_size ps_symbol_table_find(ps_symbol_table *table, char *name)
{
    ps_symbol_hash_key hash = ps_symbol_get_hash_key((char *)name);
    ps_symbol_table_size index = hash % table->size;
    if (table->symbols[index] == NULL)
    {
        if (table->trace)
            fprintf(stderr, "TRACE\tps_symbol_table_find: %s not found\n", name);
        return PS_SYMBOL_TABLE_NOT_FOUND;
    }
    if (strcmp(table->symbols[index]->name, name) != 0)
    {
        // Key collision: search for the symbol in the table
        ps_symbol_table_size start_index = index;
        do
        {
            index += 1;
            if (index >= table->size)
                index = 0; // wrap around
            if (index == start_index)
            {
                if (table->trace)
                    fprintf(stderr, "TRACE\tps_symbol_table_find: '%s' not found\n", name);
                return PS_SYMBOL_TABLE_NOT_FOUND;
            }
        } while (strcmp(table->symbols[index]->name, name) != 0);
    }
    if (table->trace)
        fprintf(stderr, "TRACE\tps_symbol_table_find: '%s' found at index %d\n", name, index);
    return index;
}

ps_symbol *ps_symbol_table_get(ps_symbol_table *table, char *name)
{
    ps_symbol_table_size index = ps_symbol_table_find(table, name);
    if (index == PS_SYMBOL_TABLE_NOT_FOUND)
        return NULL;
    return table->symbols[index];
}

ps_symbol *ps_symbol_table_add(ps_symbol_table *table, ps_symbol *symbol)
{
    // NB: refuse to add symbol if kind is AUTO or table is full
    bool error = symbol->kind == PS_SYMBOL_KIND_AUTO || table->used >= table->size;
    if (error)
    {
        if (table->debug)
            fprintf(stderr, "DEBUG\tps_symbol_table_add: '%s' => error: kind=%d, used=%d/%d\n", symbol->name,
                    symbol->kind, table->used, table->size);
        // table->error = table->used >= table->size ? PS_SYMBOL_TABLE_ERROR_FULL : PS_SYMBOL_TABLE_ERROR_INVALID;
        return NULL;
    }
    // check if symbol already exists
    if (ps_symbol_table_get(table, symbol->name) != NULL)
    {
        // table->error = PS_SYMBOL_TABLE_ERROR_EXISTS;
        return NULL;
    }
    ps_symbol_hash_key hash = ps_symbol_get_hash_key(symbol->name);
    ps_symbol_table_size index = hash % table->size;
    ps_symbol_table_size start_index = index;
    // Find an empty slot in the table
    while (table->symbols[index] != NULL)
    {
        index += 1;
        if (index >= table->size)
            index = 0; // wrap around
        if (index == start_index)
        {
            if (table->debug)
                fprintf(stderr, "DEBUG\tps_symbol_table_add: %s => table is full\n", symbol->name);
            // table->error = PS_SYMBOL_TABLE_ERROR_FULL;
            return NULL;
        }
    }
    table->symbols[index] = symbol;
    table->used += 1;
    if (table->trace)
        fprintf(stderr, "TRACE\tps_symbol_table_add: %d/%d %d '%s' \n", table->used, table->size, index, symbol->name);
    // table->error = PS_SYMBOL_TABLE_ERROR_NONE;
    return symbol;
}

// ps_symbol *ps_symbol_table_add_auto(ps_symbol_table *table, ps_symbol *symbol)
// {
//     // TODO
//     return NULL;
// }

ps_symbol *ps_symbol_table_find_string_constant(ps_symbol_table *table, char *z)
{
    ps_symbol_table_size hash = ps_symbol_get_hash_key(z);
    ps_symbol_table_size index = 0;
    ps_symbol *symbol;
    while (index < table->size)
    {
        symbol = table->symbols[index];
        if (symbol == NULL || symbol->kind != PS_SYMBOL_KIND_AUTO ||
            symbol->value->type != ps_system_string.value->data.t)
        {
            index += 1;
            continue;
        }
        if (strcmp(z, (char *)symbol->value->data.s->str) == 0)
        {
            // fprintf(stderr, "ps_symbol_table_find_string_constant: %d %s\n", index, symbol->name);
            return symbol;
        }
        index += 1;
    }
    // fprintf(stderr, "ps_symbol_table_find_string_constant: %s not found\n", z);
    return NULL;
}

ps_symbol *ps_symbol_table_add_string_constant(ps_symbol_table *table, char *z)
{
    ps_symbol *symbol;
    ps_value *value;
    ps_value_data data;
    symbol = ps_symbol_table_find_string_constant(table, z);
    if (symbol != NULL)
    {
        // fprintf(stderr, "ps_symbol_table_add_string_constant: OLD '%s' => %s\n", z, symbol->name);
        return symbol;
    }
    data.s = ps_string_create(z);
    if (data.s == NULL)
        return NULL;
    value = ps_value_alloc(ps_system_string.value->data.t, data);
    if (value == NULL)
    {
        ps_string_free(data.s);
        return NULL;
    }
    symbol = ps_symbol_alloc(PS_SYMBOL_KIND_AUTO, NULL, value);
    if (symbol == NULL)
    {
        ps_value_free(value);
        ps_string_free(data.s);
        return NULL;
    }
    if (ps_symbol_table_add(table, symbol) == NULL)
    {
        ps_symbol_free(symbol);
        ps_value_free(value);
        ps_string_free(data.s);
        return NULL;
    }
    return symbol;
}

void ps_symbol_table_dump(ps_symbol_table *table, char *title, FILE *output)
{
    if (output == NULL)
        output = stderr;
    ps_symbol *symbol;
    ps_symbol_table_size free = 0;
    ps_symbol_table_size used = 0;
    fprintf(output, "*** Symbol table %s (%d/%d) ***\n", title, table->used, table->size);
    //                        1         2         3         4         5         6         7         8
    //               12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
    //                1234 1234567890123456789012345678901 123456789 12345678 1234567890123456789012345678901
    fprintf(output, "     ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━┳━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
    fprintf(output, "     ┃Name                           ┃Kind     ┃Type    ┃Value                          ┃\n");
    fprintf(output, "┏━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━╋━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
    for (int i = 0; i < table->size; i++)
    {
        if (table->symbols[i] == NULL)
            free += 1;
        else
        {
            used += 1;
            symbol = table->symbols[i];
            char *kind_name = ps_symbol_get_kind_name(symbol->kind);
            char *type_name = symbol->value == NULL ? "NULL!" : ps_value_get_type_definition_name(symbol->value->type);
            char *value = symbol->value == NULL ? "NULL!" : ps_value_get_debug_value(symbol->value);
            fprintf(output, "┃%04d┃%-*s┃%-9s┃%-8s┃%-*s┃\n", i, PS_IDENTIFIER_LEN, symbol->name, kind_name, type_name,
                    PS_IDENTIFIER_LEN, value);
        }
    }
    fprintf(output, "┗━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━┻━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
    fprintf(output, "(free=%d/used=%d/size=%d => %s)\n", free, used, free + used,
            free + used == table->size ? "OK" : "KO");
}

/* EOF */
