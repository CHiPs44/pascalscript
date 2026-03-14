/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "ps_error.h"
#include "ps_memory.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_type_definition.h"
#include "ps_value.h"

ps_type_definition *ps_type_definition_alloc(ps_value_type type, ps_value_type base)
{
    ps_type_definition *type_def = ps_memory_malloc(PS_MEMORY_TYPE, sizeof(ps_type_definition));
    if (type_def == NULL)
        return NULL; // errno = ENOMEM
    type_def->type = type;
    type_def->base = base;
    return type_def;
}

ps_type_definition *ps_type_definition_free(ps_type_definition *type_def)
{
    if (type_def == NULL)
        return NULL;
    if (type_def->type == PS_TYPE_ENUM)
    {
        ps_memory_free(PS_MEMORY_TYPE, type_def->def.e.values);
        type_def->def.e.values = NULL;
        type_def->def.e.count = 0;
    }
    ps_memory_free(PS_MEMORY_TYPE, type_def);
    return NULL;
}

ps_type_definition *ps_type_definition_create_string(ps_string_len max)
{
    ps_type_definition *type_def = ps_type_definition_alloc(PS_TYPE_STRING, PS_TYPE_STRING);
    if (type_def == NULL)
        return NULL; // errno = ENOMEM
    type_def->def.s.max = max;
    return type_def;
}

ps_type_definition *ps_type_definition_create_enum()
{
    ps_type_definition *type_def = ps_type_definition_alloc(PS_TYPE_ENUM, PS_TYPE_UNSIGNED);
    if (type_def == NULL)
        return NULL; // errno = ENOMEM
    type_def->def.e.count = 0;
    type_def->def.e.values = NULL;
    return type_def;
}

bool ps_type_definition_set_enum_values(ps_type_definition *type_def, ps_unsigned count, ps_symbol **values)
{
    type_def->def.e.count = count;
    type_def->def.e.values = ps_memory_calloc(PS_MEMORY_TYPE, count, sizeof(ps_symbol *));
    if (type_def->def.e.values == NULL)
    {
        return false; // errno = ENOMEM
    }
    for (ps_unsigned i = 0; i < count; i++)
    {
        type_def->def.e.values[i] = values[i];
    }
    return true;
}

ps_type_definition *ps_type_definition_create_subrange_char(ps_char min, ps_char max)
{
    ps_type_definition *type_def = ps_type_definition_alloc(PS_TYPE_SUBRANGE, PS_TYPE_CHAR);
    if (type_def == NULL)
        return NULL;
    type_def->def.g.c.min = min;
    type_def->def.g.c.max = max;
    return type_def;
}

ps_type_definition *ps_type_definition_create_subrange_integer(ps_integer min, ps_integer max)
{
    ps_type_definition *type_def = ps_type_definition_alloc(PS_TYPE_SUBRANGE, PS_TYPE_INTEGER);
    if (type_def == NULL)
        return NULL;
    type_def->def.g.i.min = min;
    type_def->def.g.i.max = max;
    return type_def;
}

ps_type_definition *ps_type_definition_create_subrange_unsigned(ps_unsigned min, ps_unsigned max)
{
    ps_type_definition *type_def = ps_type_definition_alloc(PS_TYPE_SUBRANGE, PS_TYPE_UNSIGNED);
    if (type_def == NULL)
        return NULL;
    type_def->def.g.u.min = min;
    type_def->def.g.u.max = max;
    return type_def;
}

ps_type_definition *ps_type_definition_create_subrange_enum(ps_symbol *symbol_enum, ps_enum_value min,
                                                            ps_enum_value max)
{
    ps_type_definition *type_def = ps_type_definition_alloc(PS_TYPE_SUBRANGE, PS_TYPE_ENUM);
    if (type_def == NULL)
        return NULL;
    type_def->def.g.e.symbol_enum = symbol_enum;
    type_def->def.g.e.min = min;
    type_def->def.g.e.max = max;
    return type_def;
}

ps_type_definition *ps_type_definition_create_array(ps_symbol *dimension)
{
    ps_type_definition *type_def = ps_type_definition_alloc(PS_TYPE_ARRAY, PS_TYPE_ARRAY);
    if (type_def == NULL)
        return NULL;
    type_def->def.a.range = dimension;
    ps_unsigned min = 0;
    ps_unsigned max = 0;
    switch (dimension->value->data.t->base)
    {
    case PS_TYPE_CHAR:
        min = dimension->value->data.t->def.g.c.min;
        max = dimension->value->data.t->def.g.c.max;
        break;
    case PS_TYPE_UNSIGNED:
        min = dimension->value->data.t->def.g.u.min;
        max = dimension->value->data.t->def.g.u.max;
        break;
    case PS_TYPE_INTEGER:
        min = dimension->value->data.t->def.g.i.min;
        max = dimension->value->data.t->def.g.i.max;
        break;
    case PS_TYPE_ENUM:
        min = dimension->value->data.t->def.g.e.min;
        max = dimension->value->data.t->def.g.e.max;
        break;
    default:
        break;
    }
    type_def->def.a.count = max - min + 1;
    return type_def;
}

char *ps_type_definition_get_name(const ps_type_definition *type_def)
{
    static char buffer[128];
    if (type_def == NULL)
    {
        snprintf(buffer, sizeof(buffer), "NULL");
        return buffer;
    }
    char *type_name = ps_value_type_get_name(type_def->type);
    if (type_name == NULL)
    {
        snprintf(buffer, sizeof(buffer), "UNKNOWN");
        return buffer;
    }
    if (type_def->type == type_def->base)
    {
        snprintf(buffer, sizeof(buffer), "%s", type_name);
        return buffer;
    }
    char *base_name = ps_value_type_get_name(type_def->base);
    if (base_name == NULL)
    {
        snprintf(buffer, sizeof(buffer), "%s with unknown base!", type_name);
        return buffer;
    }
    switch (type_def->type)
    {
    case PS_TYPE_ENUM:
        // (One, Two, Three) => "ENUM(UNSIGNED, 3, 'One', ...)"
        snprintf(buffer, sizeof(buffer) - 1, "%s(%s, %d, '%s', ...)", type_name, base_name, type_def->def.e.count,
                 type_def->def.e.count == 0 ? "???" : type_def->def.e.values[0]->name);
        break;
    case PS_TYPE_SUBRANGE:
        // -5..24 => "SUBRANGE(INTEGER, -5..24)"
        switch (type_def->base)
        {
        case PS_TYPE_CHAR:
            snprintf(buffer, sizeof(buffer) - 1, "%s(%s, '%c'..'%c')", type_name, base_name, type_def->def.g.c.min,
                     type_def->def.g.c.max);
            break;
        case PS_TYPE_INTEGER:
            snprintf(buffer, sizeof(buffer) - 1, "%s(%s, %d..%d)", type_name, base_name, type_def->def.g.i.min,
                     type_def->def.g.i.max);
            break;
        case PS_TYPE_UNSIGNED:
            snprintf(buffer, sizeof(buffer) - 1, "%s(%s, %u..%u)", type_name, base_name, type_def->def.g.u.min,
                     type_def->def.g.u.max);
            break;
        case PS_TYPE_ENUM:
            ps_symbol **values = type_def->def.g.e.symbol_enum->value->type->value->data.t->def.e.values;
            snprintf(buffer, sizeof(buffer) - 1, "%s(%s, %s..%s)", type_name, base_name,
                     values[type_def->def.g.e.min]->name, values[type_def->def.g.e.max]->name);
            break;
        default:
            snprintf(buffer, sizeof(buffer) - 1, "%s(%s, ?..?)", type_name, base_name);
            break;
        }
        break;
    default:
        snprintf(buffer, sizeof(buffer) - 1, "%s(%s)???", type_name, base_name);
        break;
    }
    return buffer;
}

void ps_type_definition_debug(FILE *output, char *message, const ps_type_definition *type_def)
{
    if (output == NULL)
        output = stderr;
    if (message == NULL)
        message = "TYPE DEFINITION";
    fprintf(output, "%s: %s\n", message, ps_type_definition_get_name(type_def));
}
