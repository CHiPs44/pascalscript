/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_logger.h"
#include "ps_memory.h"
#include "ps_string.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_value.h"

ps_debug_level ps_symbol_table_debug_level = false;

void ps_symbol_table_log(ps_debug_level debug_level, const char *format, ...) // NOSONAR
{
    if (debug_level < ps_symbol_table_debug_level)
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
    table->used_buckets = 0;
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

ssize_t ps_symbol_table_find_used_buckets(const ps_symbol_table *table)
{
    return table == NULL ? -1 : table->used_buckets;
}

ssize_t ps_symbol_table_find_free(const ps_symbol_table *table)
{
    return table == NULL ? -1 : table->table_size - table->used_buckets;
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

ps_symbol *ps_symbol_table_find(const ps_symbol_table *table, const char *name)
{
    ps_symbol_hash_key hash = ps_symbol_get_hash_key(name);
    ssize_t index = hash % table->table_size;
    ps_bucket *bucket = table->buckets[index];
    if (bucket == NULL || bucket->used == 0)
    {
        ps_symbol_table_log(PS_DEBUG_TRACE, "TRACE\tps_symbol_table_find: '%s' not found\n", name);
        return NULL;
    }
    for (ssize_t i = 0; i < bucket->used; i++)
    {
        if (strcmp(bucket->symbols[i]->name, name) == 0)
        {
            ps_symbol_table_log(PS_DEBUG_TRACE, "TRACE\tps_symbol_table_find: '%s' found at index %d position %d\n",
                                name, index, i);
            return bucket->symbols[i];
        }
    }
    ps_symbol_table_log(PS_DEBUG_TRACE, "TRACE\tps_symbol_table_find: '%s' not found\n", name);
    return NULL;
}

ps_error ps_symbol_table_add(ps_symbol_table *table, ps_symbol *symbol)
{
    // NB: refuse to add symbol if kind is AUTO
    if (symbol->kind == PS_SYMBOL_KIND_AUTO)
        return PS_ERROR_SYMBOL_TABLE_INVALID;
    ps_symbol_hash_key hash = ps_symbol_get_hash_key(symbol->name);
    ssize_t index = hash % table->table_size;
    ps_bucket *bucket = table->buckets[index];
    // no bucket yet, allocate one
    if (bucket == NULL)
    {
        bucket = ps_symbol_table_bucket_alloc(table->bucket_size, table->bucket_more);
        if (bucket == NULL)
            return PS_ERROR_OUT_OF_MEMORY;
        table->buckets[index] = bucket;
        table->used_buckets++;
    }
    // bucket is full, grow it
    else if (bucket->used == bucket->size)
    {
        bucket->size += bucket->more;
        ps_bucket *bigger_bucket =
            ps_memory_realloc(PS_MEMORY_SYMBOL, bucket, sizeof(ps_bucket) + bucket->size * sizeof(ps_symbol *));
        if (bigger_bucket == NULL)
            return PS_ERROR_OUT_OF_MEMORY;
        table->buckets[index] = bigger_bucket;
    }
    // add symbol to bucket
    table->buckets[index]->symbols[bucket->used] = symbol;
    table->buckets[index]->used++;
    return PS_ERROR_NONE;
}

void ps_symbol_table_dump(FILE *output, char *title, const ps_symbol_table *table)
{
    ps_symbol *symbol;
    ssize_t size = 0;
    ssize_t free = 0;
    ssize_t used = 0;
    ps_symbol_hash_key hash;
    char *kind_name;
    char *type_name;
    char *value;

    if (output == NULL)
        output = stderr;
    fprintf(output, "*** Symbol table %s (%d/%d) ***\n", title, table->used_buckets, table->table_size);
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
    for (unsigned int i = 0; i < table->table_size; i++)
    {
        if (table->buckets[i] != NULL)
        {
            size += table->buckets[i]->size;
            free += table->buckets[i]->size - table->buckets[i]->used;
            used += table->buckets[i]->used;
            for (ssize_t j = 0; j < table->buckets[i]->used; j++)
            {
                symbol = table->buckets[i]->symbols[j];
                hash = ps_symbol_get_hash_key((char *)symbol->name);
                kind_name = ps_symbol_get_kind_name(symbol->kind);
                type_name = symbol->value == NULL ? "NULL!" : symbol->value->type->name;
                value = symbol->value == NULL ? "NULL!" : ps_value_get_debug_string(symbol->value);
                // clang-format off
                fprintf(output,
                        "┃%c%c%05d┃%08x%c%05d┃%-*s┃%-10s┃%-20s┃%-*s┃\n",
                        symbol->system ? 'S' : 's', symbol->allocated ? 'A' : 'a', i,
                        hash, hash % table->table_size == i ? '=' : '!', hash % table->table_size,
                        PS_IDENTIFIER_LEN, symbol->name,
                        kind_name,
                        type_name,
                        PS_IDENTIFIER_LEN, value
                );
                // clang-format on
            }
        }
    }
    fprintf(
        output,
        "┗━━━━━━━┻━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━"
        "━━━━━━━━━━━┛\n");
    fprintf(output, "(free=%d/used=%u/size=%u)\n", free, used, free + used);
}

/* EOF */
