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

 *ps_type_definition ps_type_def_integer = {.base = PS_TYPE_INTEGER, .def = {.def_subrange = {.min = ps_integer_min, .max = ps_integer_max}}};
ps_type_definition ps_type_def_unsigned = {.base = PS_TYPE_UNSIGNED};
ps_type_definition ps_type_def_real = {.base = PS_TYPE_REAL};
ps_type_definition ps_type_def_boolean = {.base = PS_TYPE_BOOLEAN};
ps_type_definition ps_type_def_char = {.base = PS_TYPE_CHAR};

ps_type_definition *ps_type_definition_create()
{
    ps_type_definition *definition = ;
    if (definition == NULL)
        return NULL;
    return definition;
}

ps_type_definition *ps_type_definition_create_base(ps_value_type type)
{
    ps_type_definition *definition = calloc(1, sizeof(ps_type_definition));
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
    symbol.kind = PS_SYMBOL_KIND_TYPE_DEFINITION;
    symbol.scope = PS_SYMBOL_SCOPE_SYSTEM;
    symbol.value.type = PS_TYPE_DEFINITION;

    // *** INTEGER ***
    strcpy(symbol.name, "INTEGER");
    symbol.value.data.t = &ps_type_def_integer;
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

const struct s_ps_type_name
{
    bool is_base_type;
    ps_value_type type;
    char *name;
} ps_type_names[] = {
    // clang-format off
    {true , PS_TYPE_NONE       , "NONE"    },
    {true , PS_TYPE_INTEGER    , "INTEGER" },
    {true , PS_TYPE_UNSIGNED   , "UNSIGNED"},
    {true , PS_TYPE_REAL       , "REAL"    },
    {true , PS_TYPE_BOOLEAN    , "BOOLEAN" },
    {true , PS_TYPE_CHAR       , "CHAR"    },
    {false, PS_TYPE_DEFINITION , "TYPE_DEF"},
    {false, PS_TYPE_ENUM       , "ENUM"    },
    {false, PS_TYPE_SUBRANGE   , "SUBRANGE"},
    {false, PS_TYPE_SET        , "SET"     },
    {false, PS_TYPE_POINTER    , "POINTER" },
    {false, PS_TYPE_STRING     , "STRING"  },
    {false, PS_TYPE_ARRAY      , "ARRAY"   },
    {false, PS_TYPE_RECORD     , "RECORD"  },
    {false, PS_TYPE_FILE       , "FILE"    },
    // clang-format on
};

char *ps_value_get_type_name(ps_type_definition *type_def)
{
    static char buffer[(PS_IDENTIFIER_MAX + 1) * 4 + 1];
    for (size_t i = 0; i < sizeof(ps_type_names) / sizeof(struct s_ps_type_name); i++)
    {
        if (type_def->base == ps_type_names[i].type)
        {
            if (ps_type_names[i].is_base_type)
                strncpy(buffer, ps_type_names[i].name, sizeof(buffer) - 1);
            else
            {
                switch (type_def->base)
                {
                case PS_TYPE_ENUM:
                    snprintf(buffer, sizeof(buffer) - 1,
                             "%s(%d, '%s', ...)",
                             ps_type_names[i].name,
                             type_def->def.def_enum.count,
                             type_def->def.def_enum.count == 0 ? "???" : type_def->def.def_enum.values[0]);
                    break;
                case PS_TYPE_SUBRANGE:
                    snprintf(buffer, sizeof(buffer) - 1,
                             "%s(%d..%d)",
                             ps_type_names[i].name,
                             type_def->def.def_subrange.min,
                             type_def->def.def_subrange.max);
                    break;
                case PS_TYPE_SET:
                    snprintf(buffer, sizeof(buffer) - 1,
                             "%s(%d, '%s', ...)",
                             ps_type_names[i].name,
                             type_def->def.def_set.count,
                             type_def->def.def_set.count == 0 ? "???" : type_def->def.def_set.values[0]);
                    break;
                case PS_TYPE_POINTER:
                    snprintf(buffer, sizeof(buffer) - 1,
                             "^%s",
                             ps_type_names[i].name,
                             type_def->def.def_pointer.type_def == NULL ? "???" : type_def->def.def_pointer.type_def->name);
                    break;
                default:
                    break;
                }
            }
            return buffer;
        }
    }
    strncpy(buffer, "UNKNOWN", sizeof(buffer) - 1);
    return buffer;
}