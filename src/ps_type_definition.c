/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "ps_error.h"
#include "ps_symbol_table.h"
#include "ps_symbol.h"
#include "ps_type_definition.h"
#include "ps_value.h"

/******************************************************************************/
/* BASE TYPE DEFINITIONS AS GLOBALS                                           */
/******************************************************************************/

//PS_TYPE_DEFINITION

ps_type_definition ps_type_def_boolean = {
    .type = PS_TYPE_BOOLEAN,
    .base = PS_TYPE_BOOLEAN,
};
ps_value ps_value_boolean = {
    .type = NULL,
    .data = {.t = &ps_type_def_boolean},
};
const ps_symbol ps_symbol_boolean = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_TYPE_DEFINITION,
    .name = "BOOLEAN",
    .value = &ps_value_boolean};

ps_type_definition ps_type_def_char = {
    .type = PS_TYPE_CHAR,
    .base = PS_TYPE_CHAR,
};
ps_value ps_value_char = {
    .type = NULL,
    .data = {.t = &ps_type_def_char},
};
const ps_symbol ps_symbol_char = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_TYPE_DEFINITION,
    .name = "CHAR",
    .value = &ps_value_char};

ps_type_definition ps_type_def_integer = {
    .type = PS_TYPE_INTEGER,
    .base = PS_TYPE_INTEGER,
};
ps_value ps_value_integer = {
    .type = NULL,
    .data = {.t = &ps_type_def_integer},
};
const ps_symbol ps_symbol_integer = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_TYPE_DEFINITION,
    .name = "INTEGER",
    .value = &ps_value_integer};

ps_type_definition ps_type_def_unsigned = {
    .type = PS_TYPE_UNSIGNED,
    .base = PS_TYPE_UNSIGNED,
};
ps_value ps_value_unsigned = {
    .type = NULL,
    .data = {.t = &ps_type_def_unsigned},
};
const ps_symbol ps_symbol_unsigned = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_TYPE_DEFINITION,
    .name = "CARDINAL",
    .value = &ps_value_unsigned};

ps_type_definition ps_type_def_real = {
    .type = PS_TYPE_REAL,
    .base = PS_TYPE_REAL,
};
ps_value ps_value_real = {
    .type = NULL,
    .data = {.t = &ps_type_def_real},
};
const ps_symbol ps_symbol_real = {
    .scope = PS_SYMBOL_SCOPE_SYSTEM,
    .kind = PS_SYMBOL_KIND_TYPE_DEFINITION,
    .name = "REAL",
    .value = &ps_value_real};

/******************************************************************************/
/* FUNCTIONS                                                                  */
/******************************************************************************/

ps_type_definition *ps_type_definition_create(ps_value_type type)
{
    ps_type_definition *definition = calloc(1, sizeof(ps_type_definition));
    if (definition == NULL)
        return NULL;
    definition->type = definition->base = type;
    return definition;
}

// ps_type_definition *ps_type_definition_create_enum(ps_unsigned count, ps_identifier *values)
// {
//     ps_type_definition *definition = ps_type_definition_create_base(PS_TYPE_ENUM);
//     if (definition == NULL)
//         return NULL;
//     definition->def.def_enum.count = count;
//     // TODO definition->def.def_enum.values = values;
//     return definition;
// }

// ps_type_definition *ps_type_definition_create_subrange(ps_integer low, ps_integer high)
// {
//     ps_type_definition *definition = ps_type_definition_create_base(PS_TYPE_SUBRANGE);
//     if (definition == NULL)
//         return NULL;
//     definition->def.def_subrange.min = low;
//     definition->def.def_subrange.max = high;
//     return definition;
// }

bool ps_type_definition_create_system_types(ps_symbol_table *table)
{
    if (ps_symbol_table_available(table) < 5)
        return false;
    ps_symbol_table_add(table, (ps_symbol *)&ps_symbol_boolean);
    ps_symbol_table_add(table, (ps_symbol *)&ps_symbol_char);
    ps_symbol_table_add(table, (ps_symbol *)&ps_symbol_integer);
    ps_symbol_table_add(table, (ps_symbol *)&ps_symbol_unsigned);
    ps_symbol_table_add(table, (ps_symbol *)&ps_symbol_real);
    return true;
}

const struct s_ps_type_name
{
    ps_value_type type;
    char *name;
} ps_type_names[] = {
    // clang-format off
    {PS_TYPE_NONE       , "NONE"    },
    {PS_TYPE_DEFINITION , "TYPE_DEF"},
    {PS_TYPE_INTEGER    , "INTEGER" },
    {PS_TYPE_UNSIGNED   , "UNSIGNED"},
    {PS_TYPE_REAL       , "REAL"    },
    {PS_TYPE_BOOLEAN    , "BOOLEAN" },
    {PS_TYPE_CHAR       , "CHAR"    },
    // {PS_TYPE_ENUM       , "ENUM"    },
    // {PS_TYPE_SUBRANGE   , "SUBRANGE"},
    // {PS_TYPE_SET        , "SET"     },
    // {PS_TYPE_POINTER    , "POINTER" },
    // {PS_TYPE_STRING     , "STRING"  },
    // {PS_TYPE_ARRAY      , "ARRAY"   },
    // {PS_TYPE_RECORD     , "RECORD"  },
    // {PS_TYPE_FILE       , "FILE"    },
    // clang-format on
};

char *ps_value_get_type_name(ps_value_type type)
{
    for (size_t i = 0; i < sizeof(ps_type_names) / sizeof(struct s_ps_type_name); i++)
    {
        if (type != ps_type_names[i].type)
        {
            continue;
        }
        return ps_type_names[i].name;
    }
    return NULL;
}

char *ps_value_get_type_definition_name(ps_type_definition *type_def)
{
    static char buffer[PS_IDENTIFIER_SIZE * 4];
    char *type_name = ps_value_get_type_name(type_def->type);
    if (type_name == NULL)
        return "UNKNOWN";
    if (type_def->type == type_def->base)
        return type_name;
    char *base_name = ps_value_get_type_name(type_def->base);
    if (base_name == NULL)
    {
        snprintf(buffer, sizeof(buffer), "%s with unknown base!", type_name);
        return buffer;
    }
    // switch (type_def->type)
    // {
    // case PS_TYPE_ENUM:
    //     // (One, Two, Three) => "ENUM(CARDINAL, 3, 'One', ...)"
    //     snprintf(buffer, sizeof(buffer) - 1,
    //              "%s(%s, %d, '%s', ...)",
    //              type_name,
    //              base_name,
    //              type_def->def.def_enum.count,
    //              type_def->def.def_enum.count == 0 ? "???" : type_def->def.def_enum.values[0]);
    //     break;
    // case PS_TYPE_SUBRANGE:
    // // -5..24 => "SUBRANGE(INTEGER, -5..24)"
    // snprintf(buffer, sizeof(buffer) - 1,
    //          "%s(%s, %d..%d)",
    //          type_name,
    //          base_name,
    //          type_def->def.def_subrange.min,
    //          type_def->def.def_subrange.max);
    // break;
    // case PS_TYPE_SET:
    //     snprintf(buffer, sizeof(buffer) - 1,
    //              "%s(%s, %d, '%s', ...)",
    //              type_name,
    //              base_name,
    //              type_def->def.def_set.count,
    //              type_def->def.def_set.count == 0 ? "???" : type_def->def.def_set.values[0]);
    //     break;
    // case PS_TYPE_POINTER:
    //     snprintf(buffer, sizeof(buffer) - 1,
    //              "^%s",
    //              ps_type_names[i].name,
    //              type_def->def.def_pointer.type_def == NULL ? "???" : type_def->def.def_pointer.type_def->name);
    //     break;
    // default:
    snprintf(buffer, sizeof(buffer) - 1, "%s(%s)???", type_name, base_name);
    //     break;
    // }
    return buffer;
}
