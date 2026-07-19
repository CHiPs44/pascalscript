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

void ps_symbol_table_log(const char *format, ...) // NOSONAR
{
    if (!ps_symbol_table_trace)
        return;
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args); // NOSONAR
    va_end(args);
}

ps_bucket *ps_symbol_table_bucket_alloc(ssize_t size, ssize_t more)
{
    ps_bucket *bucket = ps_memory_malloc(PS_MEMORY_SYMBOL, sizeof(ps_bucket) + sizeof(ps_symbol *) * size);
    if (bucket == NULL)
        return NULL;
    bucket->size = size;
    bucket->more = more;
    bucket->used = 0;
    for (ssize_t i = 0; i < size; i++)
        bucket->symbols[i] = NULL;
    return bucket;
}

ps_bucket *ps_symbol_table_bucket_free(ps_bucket *bucket, bool free_symbols)
{
    if (bucket != NULL)
    {
        if (free_symbols)
            for (ssize_t i = 0; i < bucket->used; i++)
                if (bucket->symbols[i] != NULL && bucket->symbols[i]->allocated)
                    bucket->symbols[i] = ps_symbol_free(bucket->symbols[i]);
        ps_memory_free(PS_MEMORY_SYMBOL, bucket);
    }
    return NULL;
}

void ps_symbol_table_reset(ps_symbol_table *table, bool free_symbols)
{
    for (ssize_t i = 0; i < table->table_size; i++)
    {
        if (table->buckets[i] != NULL)
            table->buckets[i] = ps_symbol_table_bucket_free(table->buckets[i], free_symbols);
    }
    table->used = 0;
}

ps_symbol_table *ps_symbol_table_alloc(ssize_t table_size, ssize_t bucket_size)
{
    ps_symbol_table *table;
    table_size = table_size > 0 ? table_size : PS_SYMBOL_TABLE_SIZE;
    table = ps_memory_malloc(PS_MEMORY_SYMBOL, sizeof(ps_symbol_table) + sizeof(ps_bucket *) * table_size);
    if (table == NULL)
        return NULL;
    table->table_size = table_size;
    table->bucket_size = bucket_size > 0 ? bucket_size : PS_SYMBOL_BUCKET_SIZE;
    ps_symbol_table_reset(table, false);
    return table;
}

void *ps_symbol_table_free(ps_symbol_table *table)
{
    if (table != NULL)
    {
        ps_symbol_table_reset(table, true);
    }
    ps_memory_free(PS_MEMORY_SYMBOL, table);
    return NULL;
}

static ps_error ps_symbol_table_add_internal(ps_symbol_table *table, ps_symbol *symbol)
{
    if (table->used >= table->size)
        return PS_ERROR_SYMBOL_TABLE_FULL;
    // check if symbol already exists
    if (ps_symbol_table_get(table, symbol->name) != NULL)
        return PS_ERROR_SYMBOL_EXISTS;
    ps_symbol_hash_key hash = ps_symbol_get_hash_key((char *)symbol->name);
    ssize_t index = hash % table->size;
    ssize_t start_index = index;
    // Find an empty slot in the table
    while (table->symbols[index] != NULL)
    {
        index += 1;
        if (index >= table->size)
            index = 0; // wrap around
        if (index == start_index)
        {
            ps_symbol_table_log("DEBUG\tps_symbol_table_add_internal: %s => table is full\n", symbol->name);
            return PS_ERROR_SYMBOL_TABLE_FULL;
        }
    }
    table->symbols[index] = symbol;
    table->used += 1;
    return PS_ERROR_NONE;
}

ssize_t ps_symbol_table_get_used(const ps_symbol_table *table)
{
    return table == NULL ? -1 : table->used;
}

ssize_t ps_symbol_table_get_free(const ps_symbol_table *table)
{
    return table == NULL ? -1 : table->table_size - table->used;
}

ps_symbol_hash_key ps_symbol_get_hash_key(const char *name)
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

ssize_t ps_symbol_table_find(const ps_symbol_table *table, const char *name)
{
    ps_symbol_hash_key hash = ps_symbol_get_hash_key(name);
    ssize_t index = hash % table->size;
    if (table->symbols[index] == NULL)
    {
        ps_symbol_table_log("TRACE\tps_symbol_table_find: '%s' not found\n", name);
        return PS_SYMBOL_TABLE_NOT_FOUND;
    }
    if (strcmp(table->symbols[index]->name, name) != 0)
    {
        // Key collision: search for the symbol in the table
        ssize_t start_index = index;
        do
        {
            index += 1;
            if (index >= table->size)
                index = 0; // wrap around
            if (index == start_index)
            {
                ps_symbol_table_log("TRACE\tps_symbol_table_find: '%s' not found\n", name);
                return PS_SYMBOL_TABLE_NOT_FOUND;
            }
            if (table->symbols[index] == NULL)
                continue;
            if (strcmp((char *)(table->symbols[index]->name), name) == 0)
            {
                ps_symbol_table_log("TRACE\tps_symbol_table_find: '%s' found at index %d\n", name, index);
                return index;
            }
        } while (true);
    }
    ps_symbol_table_log("TRACE\tps_symbol_table_find: '%s' found at index %d\n", name, index);
    return index;
}

ps_symbol *ps_symbol_table_get(const ps_symbol_table *table, const char *name)
{
    ssize_t index = ps_symbol_table_find(table, name);
    if (index == PS_SYMBOL_TABLE_NOT_FOUND)
        return NULL;
    return table->symbols[index];
}

ps_error ps_symbol_table_add(ps_symbol_table *table, ps_symbol *symbol)
{
    ps_error error = PS_ERROR_NONE;
    // NB: refuse to add symbol if kind is AUTO
    if (symbol->kind == PS_SYMBOL_KIND_AUTO)
        return PS_ERROR_SYMBOL_TABLE_INVALID;
    if (table->used >= table->size)
    {
        error = ps_symbol_table_grow(table);
        if (error != PS_ERROR_NONE)
            return error;
    }
    error = ps_symbol_table_add_internal(table, symbol);
    // fprintf(stderr, "Added symbol %s to table %p, size=%zu, used=%zu, more=%zu\n", symbol->name, (void *)table,
    //         table->size, table->used, table->more);
    return error;
}

void ps_symbol_table_dump(FILE *output, char *title, const ps_symbol_table *table)
{
    ps_symbol *symbol;
    ssize_t free = 0;
    ssize_t used = 0;
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
    fprintf(
        output,
        "┏━━━━━━━┳━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━"
        "━━━━━━━━━━━┓\n");
    fprintf(
        output,
        "┃      #┃Hash  /  index┃Name                           ┃Kind      ┃Type                ┃Value               "
        "           ┃\n");
    fprintf(
        output,
        "┣━━━━━━━╋━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━"
        "━━━━━━━━━━━┫\n");
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
                    "┃%c%c%05d┃%08x%c%05d┃%-*s┃%-10s┃%-20s┃%-*s┃\n",
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
    fprintf(
        output,
        "┗━━━━━━━┻━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━"
        "━━━━━━━━━━━┛\n");
    fprintf(output, "(free=%d/used=%u/size=%u => %s)\n", free, used, free + used,
            free + used == table->size ? "OK" : "KO");
}

/* EOF */
