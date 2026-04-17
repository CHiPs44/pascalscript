/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdbool.h>

#include "ps_memory.h"
#include "ps_type_definition.h"

ps_type_definition *ps_enum_create()
{
    ps_type_definition *type_def = ps_type_definition_alloc(PS_TYPE_ENUM, PS_TYPE_UNSIGNED);
    if (type_def == NULL)
        return NULL; // errno = ENOMEM
    type_def->def.e.count = 0;
    type_def->def.e.values = NULL;
    return type_def;
}

bool ps_enum_set_values(ps_type_definition *type_def, ps_unsigned count, ps_symbol **values)
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
