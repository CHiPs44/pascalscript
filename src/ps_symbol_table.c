/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_memory.h"
#include "ps_string.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_value.h"

bool ps_symbol_table_trace = false;

void ps_symbol_table_log(const char *format, ...)
{
    if (!ps_symbol_table_trace)
        return;
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
}

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

ps_symbol_table *ps_symbol_table_alloc(ps_symbol_table_size size)
{
    ps_symbol_table *table;
    table = ps_memory_malloc(sizeof(ps_symbol_table));
    // fprintf(stderr, "ALLOC\tSYMBOL TABLE: %p, size %d\n", table, size);
    if (table == NULL)
        return NULL;
    table->size = size > 0 ? size : PS_SYMBOL_TABLE_DEFAULT_SIZE;
    table->symbols = ps_memory_calloc(table->size, sizeof(ps_symbol *));
    if (table->symbols == NULL)
    {
        ps_memory_free(table);
        return NULL; // errno = ENOMEM
    }
    ps_symbol_table_reset(table, false);
    return table;
}

void *ps_symbol_table_free(ps_symbol_table *table)
{
    if (table != NULL)
    {
        if (table->symbols != NULL)
        {
            // fprintf(stderr, "FREE\tSYMBOL TABLE: %p, symbols at %p\n", table, table->symbols);
            ps_symbol_table_reset(table, true);
            ps_memory_free(table->symbols);
        }
        ps_memory_free(table);
    }
    return NULL;
}

ps_symbol_table_size ps_symbol_table_get_used(ps_symbol_table *table)
{
    return table == NULL ? 0 : table->used;
}

ps_symbol_table_size ps_symbol_table_get_free(ps_symbol_table *table)
{
    return table == NULL ? 0 : table->size - table->used;
}

ps_symbol_hash_key ps_symbol_get_hash_key(char *name)
{
    // DJB2, cf. https://en.wikipedia.org/wiki/Universal_hashing#Hashing_strings
    //  NB: 33 * x => 32 * x + x => x << 5 + x
    ps_symbol_hash_key hash = 5381u;
    unsigned int c = (unsigned int)(*name);
    while (c)
    {
        hash = (hash << 5) + hash + c;
        name++;
        c = (unsigned int)(*name);
    }
    return hash;
}

ps_symbol_table_size ps_symbol_table_find(ps_symbol_table *table, ps_identifier *name)
{
    ps_symbol_hash_key hash = ps_symbol_get_hash_key((char *)name);
    ps_symbol_table_size index = hash % table->size;
    if (table->symbols[index] == NULL)
    {
        ps_symbol_table_log("TRACE\tps_symbol_table_find: '%s' not found\n", (char *)name);
        return PS_SYMBOL_TABLE_NOT_FOUND;
    }
    if (strcmp((char *)(table->symbols[index]->name), (char *)name) != 0)
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
                ps_symbol_table_log("TRACE\tps_symbol_table_find: '%s' not found\n", (char *)name);
                return PS_SYMBOL_TABLE_NOT_FOUND;
            }
            if (table->symbols[index] == NULL)
                continue;
            if (strcmp((char *)(table->symbols[index]->name), (char *)name) == 0)
            {
                ps_symbol_table_log("TRACE\tps_symbol_table_find: '%s' found at index %d\n", (char *)name, index);
                return index;
            }
        } while (true);
    }
    ps_symbol_table_log("TRACE\tps_symbol_table_find: '%s' found at index %d\n", (char *)name, index);
    return index;
}

ps_symbol *ps_symbol_table_get(ps_symbol_table *table, ps_identifier *name)
{
    ps_symbol_table_size index = ps_symbol_table_find(table, name);
    if (index == PS_SYMBOL_TABLE_NOT_FOUND)
        return NULL;
    return table->symbols[index];
}

ps_symbol_table_error ps_symbol_table_add(ps_symbol_table *table, ps_symbol *symbol)
{
    // NB: refuse to add symbol if kind is AUTO or table is full
    if (symbol->kind == PS_SYMBOL_KIND_AUTO)
        return PS_SYMBOL_TABLE_ERROR_INVALID;
    if (table->used >= table->size)
        return PS_SYMBOL_TABLE_ERROR_FULL;
    // check if symbol already exists
    if (ps_symbol_table_get(table, &symbol->name) != NULL)
        return PS_SYMBOL_TABLE_ERROR_EXISTS;
    ps_symbol_hash_key hash = ps_symbol_get_hash_key((char *)symbol->name);
    ps_symbol_table_size index = hash % table->size;
    ps_symbol_table_size start_index = index;
    fprintf(stderr, "ADD\tps_symbol_table_add: '%s' at index %d (hash %08x)\n", symbol->name, index, hash);
    // Find an empty slot in the table
    while (table->symbols[index] != NULL)
    {
        index += 1;
        if (index >= table->size)
            index = 0; // wrap around
        if (index == start_index)
        {
            ps_symbol_table_log("DEBUG\tps_symbol_table_add: %s => table is full\n", symbol->name);
            return PS_SYMBOL_TABLE_ERROR_FULL;
        }
    }
    table->symbols[index] = symbol;
    table->used += 1;
    ps_symbol_table_log("TRACE\tps_symbol_table_add: %d/%d %d '%s' \n", table->used, table->size, index, symbol->name);
    return PS_SYMBOL_TABLE_ERROR_NONE;
}

void ps_symbol_table_dump(FILE *output, char *title, ps_symbol_table *table)
{
    ps_symbol *symbol;
    ps_symbol_table_size free = 0;
    ps_symbol_table_size used = 0;
    ps_symbol_hash_key hash;
    char *kind_name;
    char *type_name;
    char *value;

    if (output == NULL)
        output = stderr;
    fprintf(output, "*** Symbol table %s (%d/%d) ***\n", title, table->used, table->size);
    //                        1         2         3         4         5         6         7         8         9
    //               1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901
    //                1234 1234567890123456789012345678901 1234567890 1234567890 1234567890123456789012345678901
    fprintf(output, "┏━━━━━━━┳━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━┳━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
    fprintf(output, "┃      #┃Hash / index  ┃Name                           ┃Kind      ┃Type      ┃Value                          ┃\n");
    fprintf(output, "┣━━━━━━━╋━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━╋━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
    for (unsigned int i = 0; i < table->size; i++)
    {
        if (table->symbols[i] == NULL)
            free += 1;
        else
        {
            used += 1;
            symbol = table->symbols[i];
            hash = ps_symbol_get_hash_key((char *)symbol->name);
            kind_name = ps_symbol_get_kind_name(symbol->kind);
            type_name = symbol->value == NULL ? "NULL!" : symbol->value->type->name;
            value = symbol->value == NULL ? "NULL!" : ps_value_get_debug_string(symbol->value);
            // clang-format off
            fprintf(output, 
                    "┃%c%c%05d┃%08x%c%05d┃%-*s┃%-10s┃%-10s┃%-*s┃\n", 
                    symbol->system ? 'S' : 's', symbol->allocated ? 'A' : 'a', i, 
                    hash, hash % table->size == i ? '=' : '!', hash % table->size,
                    PS_IDENTIFIER_LEN, symbol->name, 
                    kind_name, 
                    type_name,
                    PS_IDENTIFIER_LEN, value
            );
            // clang-format on
        }
    }
    fprintf(output, "┗━━━━━━━┻━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━┻━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
    fprintf(output, "(free=%d/used=%d/size=%d => %s)\n", free, used, free + used,
            free + used == table->size ? "OK" : "KO");
}

/* EOF */
