/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_value_types.h"

char *ps_type_names[] = {
    // clang-format off
//   1234567890    1234567890    1234567890    1234567890
    "NONE"      , "REAL"      , "INTEGER"   , "UNSIGNED"  , 
    "BOOLEAN"   , "CHAR"      , "STRING"    , "SET"       , 
    "TYPE_DEF"  , "EXECUTABLE", "SUBRANGE"  , "ENUM"      , 
    "POINTER"   , "ARRAY"     , "RECORD"    , "FILE"
    // clang-format on
};

char *ps_value_type_get_name(ps_value_type type)
{
    if (type >= PS_TYPE_NONE && type <= PS_TYPE_FILE)
        return ps_type_names[type];
    return NULL;
}
