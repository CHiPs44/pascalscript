/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "ps_symbol.h"
#include "ps_value.h"

/**
 * @brief Normalize symbol (=> UPPERCASE) in place (no string copy)
 *
 * @param name
 */
void symbol_normalize_name(char *name)
{
    while (*name)
    {
        /* a-z => A-Z */
        if (*name >= 'a' && *name <= 'z')
        {
            *name -= ('a' - 'A');
        }
        name++;
    }
}

char *symbol_get_kind_name(kind_t kind)
{
    char *kind_name;
    switch (kind)
    {
    case KIND_FREE:
        kind_name = "FREE";
        break;
    case KIND_AUTO:
        kind_name = "AUTO";
        break;
    case KIND_CONSTANT:
        kind_name = "CONSTANT";
        break;
    case KIND_VARIABLE:
        kind_name = "VARIABLE";
        break;
    default:
        kind_name = "?";
        break;
    }
    return kind_name;
}

char *symbol_get_scope_name(ps_scope_t scope)
{
    static char scope_name[8 + 1];
    if (scope == PS_SCOPE_GLOBAL)
        snprintf(scope_name, 15, "GLOBAL");
    else
        snprintf(scope_name, 15, "LOCAL%03d", scope);
    return scope_name;
}

void symbol_dump(symbol_t *symbol)
{
    char *kind_name = symbol_get_kind_name(symbol->kind);
    char *scope_name = symbol_get_scope_name(symbol->scope);
    char *type_name = value_get_type_name(symbol->value.type);
    char *buffer = value_get_value(&symbol->value);
    fprintf(stderr,
            "SYMBOL: name=%s, kind=%s, scope=%s, type=%s, size=%ld, value=%s\n",
            symbol->name, kind_name, scope_name, type_name, symbol->value.size, buffer);
}

/* EOF */
