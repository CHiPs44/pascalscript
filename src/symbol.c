/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>
#include "symbol.h"

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
    char *kind;
    /*          12345678 */
    switch (kind)
    {
    case KIND_UNKNOWN:
        kind = "UNKNOWN ";
        break;
    case KIND_AUTO:
        kind = "AUTO    ";
        break;
    case KIND_FREE:
        kind = "FREE    ";
        break;
    case KIND_CONSTANT:
        kind = "CONSTANT";
        break;
    case KIND_VARIABLE:
        kind = "VARIABLE";
        break;
    default:
        kind = "????????";
        break;
    }
    return kind;
}

char *symbol_get_type_name(type_t type)
{
    char *type;
    /*          12345678 */
    switch (type)
    {
    case TYPE_NONE:
        type = "NONE    ";
        break;
    case TYPE_INTEGER:
        type = "INTEGER ";
        break;
    default:
        type = "????????";
        break;
    }
    return type;
}

void symbol_dump(symbol_t *symbol)
{
    char *kind = symbol_get_kind_name(symbol->kind);
    char *type = symbol_get_type_name(symbol->type);
    char value[32];
    switch (symbol->type)
    {
    case TYPE_NONE:
        snprintf(value, 31, "?");
        break;
    case TYPE_INTEGER:
        snprintf(value, 31, "%d / %08x", symbol->value.i);
        break;
    default:
        snprintf(value, 31, "?");
        break;
    }
    fprintf(stderr,
            "SYMBOL: name=%s, type=%s, kind=%s, size=%d, value=%s\n",
            symbol->name, kind, type, symbol->size, value);
}

/* EOF */
