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
        table->symbols[i] = NULL;
    }
    table->used = 0;
}

ps_symbol_table *ps_symbol_table_init(ps_symbol_table *table)
{
    if (table == NULL)
    {
        table = calloc(1, sizeof(ps_symbol_table));
        if (table == NULL)
            return NULL;
    }
    table->size = sizeof(table->symbols) / sizeof(ps_symbol *);
    ps_symbol_table_reset(table);
    return table;
}

void ps_symbol_table_done(ps_symbol_table *table)
{
    free(table);
}

ps_symbol_table_size ps_symbol_table_used(ps_symbol_table *table)
{
    return table->used;
}

ps_symbol_table_size ps_symbol_table_available(ps_symbol_table *table)
{
    return table->size - table->used;
}

ps_symbol_table_size s_symbol_table_find(ps_symbol_table *table, char *name)
{
    ps_symbol_table_size index = 0;
    // ps_symbol_normalize_name(name);
    while (index < table->size)
    {
        if (table->symbols[index] != NULL && strcmp(name, table->symbols[index]->name) == 0)
        {
            return index;
        }
        index += 1;
    }
    return PS_SYMBOL_TABLE_ERROR_NOT_FOUND;
}

ps_symbol *ps_symbol_table_get(ps_symbol_table *table, char *name)
{
    ps_symbol_table_size index = ps_symbol_table_find(table, name);
    if (index == PS_SYMBOL_TABLE_ERROR_NOT_FOUND)
        return NULL;
    return table->symbols[index];
}

ps_symbol *ps_symbol_table_add(ps_symbol_table *table, ps_symbol *symbol)
{
    if (table->used >= table->size)
        return NULL;
    if (ps_symbol_table_get(table, symbol->name) != NULL)
        return NULL;
    // Find first free location (table->used *must* be accurate)
    ps_symbol_table_size index = 0;
    while (table->symbols[index] != NULL)
    {
        index += 1;
    }
    if (symbol->kind == PS_SYMBOL_KIND_AUTO)
        snprintf(symbol->name, PS_IDENTIFIER_LEN, PS_SYMBOL_AUTO_FORMAT, index);
    table->symbols[index] = symbol;
    table->used += 1;
    return symbol;
}

/**
 * @brief Delete symbol by name
 */
ps_symbol *ps_symbol_table_delete(ps_symbol_table *table, ps_identifier *name)
{
    ps_symbol_table_size index = ps_symbol_table_find(table, name);
    if (index == PS_SYMBOL_TABLE_ERROR_NOT_FOUND)
        return NULL;
    ps_symbol *symbol = table->symbols[index];
    table->symbols[index] = NULL;
    table->used -= 1;
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
    //                        1         2         3         4         5         6         7         8         9        10
    //               1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456
    //                1234 1234567890123456789012345678901 12345678 12345678 12345678 12345678 1234567890123456789012345678901
    fprintf(output, "     ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
    fprintf(output, "     ┃Name                           ┃Kind    ┃Scope   ┃Type    ┃Value                          ┃\n");
    fprintf(output, "┏━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
    for (int i = 0; i < table->size; i++)
    {
        if (table->symbols[i] == NULL)
        {
            free += 1;
        }
        else
        {
            used += 1;
            symbol = table->symbols[i];
            char *kind_name = ps_symbol_get_kind_name(symbol->kind);
            char *scope_name = ps_symbol_get_scope_name(symbol->scope);
            char *type_name = ps_value_get_type_definition_name(symbol->value->type);
            char *value = ps_value_get_debug_value(symbol->value);
            fprintf(output, "┃%04d┃%-*s┃%-8s┃%-8s┃%-8s┃%-*s┃\n",
                    i, PS_IDENTIFIER_LEN, symbol->name, kind_name, scope_name, type_name, PS_IDENTIFIER_LEN, value);
        }
    }
    fprintf(output, "┗━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━┻━━━━━━━━┻━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
    fprintf(output, "(free=%d/used=%d/size=%d => %s)\n", free, used, free + used, free + used == table->size ? "OK" : "KO");
}

/* EOF */
