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

// clang-format off
ps_value_type_flags ps_value_type_flags_all[] = {
    /* 00 PS_TYPE_NONE       */ {.is_base = 0, .is_numeric = 0, .is_ordinal = 0, .is_scalar = 0, .is_signed = 0, .is_reference = 0}, 
    /* 01 PS_TYPE_REAL       */ {.is_base = 1, .is_numeric = 1, .is_ordinal = 0, .is_scalar = 1, .is_signed = 1, .is_reference = 0}, 
    /* 02 PS_TYPE_INTEGER    */ {.is_base = 1, .is_numeric = 1, .is_ordinal = 1, .is_scalar = 1, .is_signed = 1, .is_reference = 0}, 
    /* 03 PS_TYPE_UNSIGNED   */ {.is_base = 1, .is_numeric = 1, .is_ordinal = 1, .is_scalar = 1, .is_signed = 0, .is_reference = 0}, 
    /* 04 PS_TYPE_BOOLEAN    */ {.is_base = 1, .is_numeric = 0, .is_ordinal = 1, .is_scalar = 1, .is_signed = 0, .is_reference = 0}, 
    /* 05 PS_TYPE_CHAR       */ {.is_base = 1, .is_numeric = 0, .is_ordinal = 1, .is_scalar = 1, .is_signed = 0, .is_reference = 0}, 
    /* 06 PS_TYPE_STRING     */ {.is_base = 1, .is_numeric = 0, .is_ordinal = 0, .is_scalar = 0, .is_signed = 0, .is_reference = 1}, 
    /* 07 PS_TYPE_DEFINITION */ {.is_base = 0, .is_numeric = 0, .is_ordinal = 0, .is_scalar = 0, .is_signed = 0, .is_reference = 0}, 
    /* 08 PS_TYPE_SUBRANGE   */ {.is_base = 0, .is_numeric = 1, .is_ordinal = 1, .is_scalar = 0, .is_signed = 0, .is_reference = 0}, 
    /* 09 PS_TYPE_ENUM       */ {.is_base = 0, .is_numeric = 0, .is_ordinal = 1, .is_scalar = 1, .is_signed = 0, .is_reference = 0}, 
    /* 10 PS_TYPE_SET        */ {.is_base = 0, .is_numeric = 0, .is_ordinal = 0, .is_scalar = 0, .is_signed = 0, .is_reference = 0}, 
    /* 11 PS_TYPE_POINTER    */ {.is_base = 0, .is_numeric = 0, .is_ordinal = 0, .is_scalar = 0, .is_signed = 0, .is_reference = 1}, 
    /* 12 PS_TYPE_ARRAY      */ {.is_base = 0, .is_numeric = 0, .is_ordinal = 0, .is_scalar = 0, .is_signed = 0, .is_reference = 1}, 
    /* 13 PS_TYPE_RECORD     */ {.is_base = 0, .is_numeric = 0, .is_ordinal = 0, .is_scalar = 0, .is_signed = 0, .is_reference = 1}, 
    /* 14 PS_TYPE_FILE       */ {.is_base = 0, .is_numeric = 0, .is_ordinal = 0, .is_scalar = 0, .is_signed = 0, .is_reference = 1}, 
    /* 15 PS_TYPE_OBJECT     */ {.is_base = 0, .is_numeric = 0, .is_ordinal = 0, .is_scalar = 0, .is_signed = 0, .is_reference = 1},  
};
// clang-format on
#define PS_VALUE_TYPE_SIZE sizeof(ps_value_type_flags_all)

ps_type_definition *ps_type_definition_create(ps_value_type type)
{
    ps_type_definition *type_def = calloc(1, sizeof(ps_type_definition));
    if (type_def == NULL)
        return NULL; // errno = ENOMEM
    type_def->type = type_def->base = type;
    return type_def;
}

// ps_type_definition *ps_type_definition_create_subrange(ps_symbol *def, ps_value_data min, ps_value_data max)
// {
//     ps_type_definition *type_def = ps_type_definition_create(PS_TYPE_SUBRANGE);
//     if (type_def == NULL)
//         return NULL;
//     type_def->def.def_subrange.def = def;
//     type_def->def.def_subrange.min = min;
//     type_def->def.def_subrange.max = max;
//     return type_def;
// }

// ps_type_definition *ps_type_definition_create_enum(ps_unsigned count, ps_symbol *values)
// {
//     ps_type_definition *type_def = ps_type_definition_create(PS_TYPE_ENUM);
//     if (type_def == NULL)
//         return NULL; // errno = ENOMEM
//     type_def->def.def_enum.count = count;
//     type_def->def.def_enum.values = calloc(count, sizeof(ps_symbol *));
//     if (type_def->def.def_enum.values == NULL)
//     {
//         free(type_def);
//         return NULL; // errno = ENOMEM
//     }
//     for (ps_unsigned i = 0; i < count; i++)
//     {
//         type_def->def.def_enum.values[i] = values[i];
//     }
//     return type_def;
// }

char *ps_type_names[] = {
    // clang-format off
//   1234567890    1234567890    1234567890    1234567890
    "NONE"      , "REAL"      , "INTEGER"   , "UNSIGNED"  , 
    "BOOLEAN"   , "CHAR"      , "STRING"    , "TYPE_DEF"  , 
    "EXECUTABLE", "SUBRANGE"  , "ENUM"      , "SET"       , 
    "POINTER"   , "ARRAY"     , "RECORD"    , "FILE"
    // clang-format on
};

char *ps_value_get_type_name(ps_value_type type)
{
    if (type >= PS_TYPE_NONE && type <= PS_TYPE_FILE)
        return ps_type_names[type];
    return NULL;
}

char *ps_value_get_type_definition_name(ps_type_definition *type_def)
{
    static char buffer[PS_IDENTIFIER_SIZE * 4];
    if (type_def == NULL)
        return "NULL";
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
