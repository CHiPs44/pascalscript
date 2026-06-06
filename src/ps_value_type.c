/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_value_types.h"

char *ps_type_names[] = {"UNKNOWN", "NODE",   "REAL",     "INTEGER",    "UNSIGNED", "BOOLEAN",
                         "CHAR",    "STRING", "TYPE_DEF", "EXECUTABLE", "SUBRANGE", "ENUM",
                         "ARRAY",   "SET",    "POINTER",  "RECORD",     "FILE"};

char *ps_value_type_get_name(ps_value_type type)
{
    static char error[32];
    if (type < 0 || type > PS_TYPE_FILE)
    {
        snprintf(error, sizeof(error), "ERROR(%d)", type);
        return error;
    }
    return ps_type_names[type];
}
