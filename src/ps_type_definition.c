/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "ps_error.h"
#include "ps_type_definition.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"

ps_type_definition *ps_type_definition_create()
{
    ps_type_definition *definition = calloc(1, sizeof(ps_type_definition));
    if (definition == NULL)
        return NULL;
    return definition;
}

ps_type_definition *ps_type_definition_create_base(ps_value_type type)
{
    ps_type_definition *definition = ps_type_definition_create();
    if (definition == NULL)
        return NULL;
    definition->base = type;
    return definition;
}

ps_type_definition *ps_type_definition_create_enum(ps_unsigned count, ps_identifier *values)
{
    ps_type_definition *definition = ps_type_definition_create_base(PS_TYPE_ENUM);
    if (definition == NULL)
        return NULL;
    definition->def.def_enum.count = count;
    // TODO definition->def.def_enum.values = values;
    return definition;
}

ps_type_definition *ps_type_definition_create_subrange(ps_integer low, ps_integer high)
{
    ps_type_definition *definition = ps_type_definition_create_base(PS_TYPE_SUBRANGE);
    if (definition == NULL)
        return NULL;
    definition->def.def_subrange.min = low;
    definition->def.def_subrange.max = high;
    return definition;
}

bool ps_type_definition_create_system_types(ps_symbol_table *symbol_table)
{
    ps_symbol symbol;
    symbol.kind = PS_SYMBOL_KIND_TYPE;
    symbol.scope = PS_SCOPE_SYSTEM;
    symbol.value.type = PS_TYPE_DEFINITION;

    // *** INTEGER ***
    ps_type_definition *ps_type_definition_integer = ps_type_definition_create_base(PS_TYPE_INTEGER);
    if (ps_type_definition_integer == NULL)
        return false;
    ps_type_definition_integer->def.def_subrange.min = ps_integer_min;
    ps_type_definition_integer->def.def_subrange.max = ps_integer_max;
    strcpy(symbol.name, "INTEGER");
    symbol.value.data.t = ps_type_definition_integer;
    ps_symbol_table_add(symbol_table, &symbol);

    // *** UNSIGNED / CARDINAL ***
    ps_type_definition *ps_type_definition_unsigned = ps_type_definition_create_base(PS_TYPE_UNSIGNED);
    if (ps_type_definition_unsigned == NULL)
        return false;
    ps_type_definition_unsigned->def.def_subrange.min = 0;
    ps_type_definition_unsigned->def.def_subrange.max = ps_unsigned_max;
    strcpy(symbol.name, "CARDINAL");
    symbol.value.data.t = ps_type_definition_unsigned;
    ps_symbol_table_add(symbol_table, &symbol);

    // *** BOOLEAN ***
    ps_type_definition *ps_type_definition_boolean = ps_type_definition_create_base(PS_TYPE_BOOLEAN);
    if (ps_type_definition_boolean == NULL)
        return false;
    ps_type_definition_boolean->def.def_subrange.min = 0;
    ps_type_definition_boolean->def.def_subrange.max = 1;
    strcpy(symbol.name, "BOOLEAN");
    symbol.value.data.t = ps_type_definition_boolean;
    ps_symbol_table_add(symbol_table, &symbol);

    // *** CHAR ***
    ps_type_definition *ps_type_definition_char = ps_type_definition_create_base(PS_TYPE_CHAR);
    if (ps_type_definition_char == NULL)
        return false;
    ps_type_definition_char->def.def_subrange.min = 0;
    ps_type_definition_char->def.def_subrange.max = ps_char_max;
    strcpy(symbol.name, "CHAR");
    symbol.value.data.t = ps_type_definition_char;
    ps_symbol_table_add(symbol_table, &symbol);

    // *** REAL ***
    ps_type_definition *ps_type_definition_real = ps_type_definition_create_base(PS_TYPE_REAL);
    if (ps_type_definition_real == NULL)
        return false;
    strcpy(symbol.name, "REAL");
    symbol.value.data.t = ps_type_definition_real;
    ps_symbol_table_add(symbol_table, &symbol);

    return true;
}
