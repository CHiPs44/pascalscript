/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_config.h"
#include "ps_symbol.h"
#include "ps_symbol_list.h"
#include "ps_memory.h"

ps_symbol_list *ps_symbol_list_alloc(int size, int more)
{
    ps_symbol_list *list = ps_memory_malloc(PS_MEMORY_PARSER, sizeof(ps_symbol_list));
    if (list == NULL)
        return NULL; // errno = ENOMEMenum_values
    list->values = ps_memory_calloc(PS_MEMORY_PARSER, size, sizeof(ps_symbol *));
    if (list->values == NULL)
    {
        ps_memory_free(PS_MEMORY_PARSER, list);
        return NULL; // errno = ENOMEM
    }
    list->size = size;
    list->more = more;
    list->used = 0;
    return list;
}

void ps_symbol_list_free(ps_symbol_list *list, bool free_symbols)
{
    if (free_symbols)
        for (ps_unsigned i = 0; i < list->used; i++)
        {
            ps_symbol_free(list->values[i]);
        }
    ps_memory_free(PS_MEMORY_PARSER, list->values);
    ps_memory_free(PS_MEMORY_PARSER, list);
}

bool ps_symbol_list_grow(ps_symbol_list *list)
{
    // still room for new values?
    if (list->used < list->size)
        return true;
    // grow list
    if (list->size + list->more > 256)
        return false;
    ps_symbol **new_values =
        ps_memory_realloc(PS_MEMORY_PARSER, list->values, (list->size + list->more) * sizeof(ps_symbol *));
    if (new_values == NULL)
        return false; // errno = ENOMEM
    list->values = new_values;
    list->size += list->more;
    return true;
}

bool ps_symbol_list_find(const ps_symbol_list *list, const char *name)
{
    for (ps_unsigned i = 0; i < list->used; i++)
        if (strcmp(name, list->values[i]->name) == 0)
            return true;
    return false;
}

ps_symbol *ps_symbol_list_add(ps_symbol_list *list, ps_symbol *type_symbol, const char *name)
{
    // Create a new symbol for the enumeration value
    ps_value *value = ps_value_alloc(type_symbol, (ps_value_data){.u = list->used});
    if (value == NULL)
        return NULL;
    ps_symbol *symbol = ps_symbol_alloc(PS_SYMBOL_KIND_CONSTANT, name, value);
    if (symbol == NULL)
    {
        ps_value_free(value);
        return NULL;
    }
    if (!ps_symbol_list_grow(list))
        return NULL;
    list->values[list->used] = symbol;
    list->used += 1;
    return symbol;
}
