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

void ps_symbol_table_reset(ps_symbol_table *table, bool free_symbols)
{
    for (int i = 0; i < table->size; i++)
    {
        if (table->symbols[i] != NULL)
        {
            if (free_symbols && table->symbols[i]->allocated)
            {
                table->symbols[i] = ps_symbol_free(table->symbols[i]);
            }
            table->symbols[i] = NULL;
        }
    }
    table->used = 0;
}

ps_symbol_table *ps_symbol_table_alloc(ps_symbol_table_size size, ps_symbol_table_size more)
{
    ps_symbol_table *table;
    table = ps_memory_malloc(PS_MEMORY_SYMBOL, sizeof(ps_symbol_table));
    if (table == NULL)
        return NULL;
    table->size = size > 0 ? size : PS_SYMBOL_TABLE_DEFAULT_SIZE;
    table->more = more > 0 ? more : PS_SYMBOL_TABLE_DEFAULT_MORE;
    table->symbols = ps_memory_calloc(PS_MEMORY_SYMBOL, table->size, sizeof(ps_symbol *));
    if (table->symbols == NULL)
    {
        ps_memory_free(PS_MEMORY_SYMBOL, table);
        return NULL; // errno = ENOMEM
    }
    ps_symbol_table_reset(table, false);
    return table;
}

void *ps_symbol_table_free(ps_symbol_table *table)
{
    if (table != NULL && table->symbols != NULL)
    {
        ps_symbol_table_reset(table, true);
        ps_memory_free(PS_MEMORY_SYMBOL, table->symbols);
    }
    ps_memory_free(PS_MEMORY_SYMBOL, table);
    return NULL;
}

ps_symbol_table_error ps_symbol_table_add_internal(ps_symbol_table *table, ps_symbol *symbol)
{
    if (table->used >= table->size)
        return PS_SYMBOL_TABLE_ERROR_FULL;
    // check if symbol already exists
    if (ps_symbol_table_get(table, symbol->name) != NULL)
        return PS_SYMBOL_TABLE_ERROR_EXISTS;
    ps_symbol_hash_key hash = ps_symbol_get_hash_key((char *)symbol->name);
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
            ps_symbol_table_log("DEBUG\tps_symbol_table_add_internal: %s => table is full\n", symbol->name);
            return PS_SYMBOL_TABLE_ERROR_FULL;
        }
    }
    table->symbols[index] = symbol;
    table->used += 1;
    return PS_SYMBOL_TABLE_ERROR_NONE;
}

ps_symbol_table_error ps_symbol_table_grow(ps_symbol_table *table)
{
    ps_symbol_table_error error = PS_SYMBOL_TABLE_ERROR_NONE;
    ps_symbol_table_size new_size = table->size + table->more;
    ps_symbol_table_size old_size = table->size;
    ps_symbol_table_size old_used = table->used;
    ps_symbol **new_symbols = ps_memory_calloc(PS_MEMORY_SYMBOL, new_size, sizeof(ps_symbol *));
    if (new_symbols == NULL)
        return PS_SYMBOL_TABLE_ERROR_FULL;
    ps_symbol **old_symbols = table->symbols;
    table->symbols = new_symbols;
    table->size = new_size;
    table->used = 0;
    ps_symbol_table_reset(table, false);
    for (ps_symbol_table_size i = 0; i < old_size; i++)
    {
        if (old_symbols[i] != NULL)
        {
            error = ps_symbol_table_add_internal(table, old_symbols[i]);
            if (error != PS_SYMBOL_TABLE_ERROR_NONE)
                goto cleanup;
        }
    }
    if (table->used != old_used)
    {
        error = PS_SYMBOL_TABLE_ERROR_INVALID;
        goto cleanup;
    }
    ps_memory_free(PS_MEMORY_SYMBOL, old_symbols);
    ps_symbol_table_log("*** GROW from %u to %u ***\n", old_size, new_size);
    return PS_SYMBOL_TABLE_ERROR_NONE;
cleanup:
    if (new_symbols != NULL)
        ps_memory_free(PS_MEMORY_SYMBOL, new_symbols);
    table->symbols = old_symbols;
    table->size = old_size;
    table->used = old_used;
    return error;
}

ps_symbol_table_size ps_symbol_table_get_used(const ps_symbol_table *table)
{
    return table == NULL ? 0 : table->used;
}

ps_symbol_table_size ps_symbol_table_get_free(const ps_symbol_table *table)
{
    return table == NULL ? 0 : table->size - table->used;
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

ps_symbol_table_size ps_symbol_table_find(const ps_symbol_table *table, const char *name)
{
    ps_symbol_hash_key hash = ps_symbol_get_hash_key(name);
    ps_symbol_table_size index = hash % table->size;
    if (table->symbols[index] == NULL)
    {
        ps_symbol_table_log("TRACE\tps_symbol_table_find: '%s' not found\n", name);
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
    ps_symbol_table_size index = ps_symbol_table_find(table, name);
    if (index == PS_SYMBOL_TABLE_NOT_FOUND)
        return NULL;
    return table->symbols[index];
}

ps_symbol_table_error ps_symbol_table_add(ps_symbol_table *table, ps_symbol *symbol)
{
    ps_symbol_table_error error = PS_SYMBOL_TABLE_ERROR_NONE;
    // NB: refuse to add symbol if kind is AUTO
    if (symbol->kind == PS_SYMBOL_KIND_AUTO)
        return PS_SYMBOL_TABLE_ERROR_INVALID;
    if (table->used >= table->size)
    {
        error = ps_symbol_table_grow(table);
        if (error != PS_SYMBOL_TABLE_ERROR_NONE)
            return error;
    }
    error = ps_symbol_table_add_internal(table, symbol);
    return error;
}

ps_symbol_table_error ps_symbol_table_remove(ps_symbol_table *table, ps_symbol *symbol)
{
    ps_symbol_table_log("TRACE\tps_symbol_table_remove:\tBEGIN\t%d/%d %d '%s' \n", table->used, table->size, index,
                        symbol->name);
    ps_symbol_table_size index = ps_symbol_table_find(table, symbol->name);
    if (index == PS_SYMBOL_TABLE_NOT_FOUND)
        return PS_SYMBOL_TABLE_ERROR_NOT_FOUND;
    table->symbols[index] = NULL;
    table->used -= 1;
    ps_symbol_table_log("TRACE\tps_symbol_table_remove:\tEND\t%d/%d %d '%s' \n", table->used, table->size, index,
                        symbol->name);
    return PS_SYMBOL_TABLE_ERROR_NONE;
}

void ps_symbol_table_dump(FILE *output, char *title, const ps_symbol_table *table)
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
