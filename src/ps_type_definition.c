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

ps_type_definition *ps_type_definition_create(char *name)
{
    ps_type_definition *definition = calloc(1, sizeof(ps_type_definition));
    if (definition == NULL)
        return NULL;
    strncpy(definition->name, PS_IDENTIFIER_MAX, name);
    return definition;
}

ps_type_definition *ps_type_definition_create_base(char *name, ps_value_type type)
{
    ps_type_definition *definition = ps_type_definition_create(name);
    if (definition == NULL)
        return NULL;
    definition->base = type;
    return definition;
}

ps_type_definition *ps_type_definition_create_enum(char *name, ps_unsigned count, char *values[PS_IDENTIFIER_MAX + 1])
{
    ps_type_definition *definition = ps_type_definition_create(name);
    if (definition == NULL)
        return NULL;
    definition->base = PS_TYPE_ENUM;
    definition->def.def_enum.count = count;
    // ??? definition->def.def_enum.values = values;
    return definition;
}

ps_type_definition *ps_type_definition_create_subrange(char *name, ps_integer low, ps_integer high)
{
    ps_type_definition *definition = ps_type_definition_create(name);
    if (definition == NULL)
        return NULL;
    definition->base = PS_TYPE_SUBRANGE;
    definition->def.def_subrange.low = low;
    definition->def.def_subrange.high = high;
    return definition;
}
