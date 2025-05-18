/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "ps_error.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_type_definition.h"
#include "ps_value.h"

ps_type_definition *ps_type_definition_create(ps_value_type type)
{
    ps_type_definition *definition = calloc(1, sizeof(ps_type_definition));
    if (definition == NULL)
        return NULL; // errno = ENOMEM
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

const struct s_ps_type_name
{
    ps_value_type type;
    char *name;
} ps_type_names[] = {
    // clang-format off
    //                     12345678
    {PS_TYPE_NONE       , "NONE"    },
    {PS_TYPE_DEFINITION , "TYPE_DEF"},
    {PS_TYPE_REAL       , "REAL"    },
    {PS_TYPE_INTEGER    , "INTEGER" },
    {PS_TYPE_UNSIGNED   , "UNSIGNED"},
    {PS_TYPE_BOOLEAN    , "BOOLEAN" },
    {PS_TYPE_CHAR       , "CHAR"    },
    {PS_TYPE_ENUM       , "ENUM"    },
    {PS_TYPE_SUBRANGE   , "SUBRANGE"},
    {PS_TYPE_SET        , "SET"     },
    {PS_TYPE_POINTER    , "POINTER" },
    {PS_TYPE_STRING     , "STRING"  },
    {PS_TYPE_ARRAY      , "ARRAY"   },
    {PS_TYPE_RECORD     , "RECORD"  },
    {PS_TYPE_FILE       , "FILE"    },
    {PS_TYPE_OBJECT     , "OBJECT"  },
    // clang-format on
};

char *ps_value_get_type_name(ps_value_type type)
{
    for (size_t i = 0; i < sizeof(ps_type_names) / sizeof(struct s_ps_type_name); i++)
    {
        if (type == ps_type_names[i].type)
            return ps_type_names[i].name;
    }
    return NULL;
}

char *ps_value_get_type_definition_name(ps_type_definition *type_def)
{
    static char buffer[PS_IDENTIFIER_SIZE * 4];
    if (type_def == NULL)
    {
        return "NULL";
    }
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
    //     // (One, Two, Three) => "ENUM(UNSIGNED, 3, 'One', ...)"
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
