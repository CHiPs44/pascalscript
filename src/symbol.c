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
    char *kind_name;
    /*          12345678 */
    switch (kind)
    {
    case KIND_UNKNOWN:
        kind_name = "UNKNOWN ";
        break;
    case KIND_AUTO:
        kind_name = "AUTO    ";
        break;
    case KIND_FREE:
        kind_name = "FREE    ";
        break;
    case KIND_CONSTANT:
        kind_name = "CONSTANT";
        break;
    case KIND_VARIABLE:
        kind_name = "VARIABLE";
        break;
    default:
        kind_name = "????????";
        break;
    }
    return kind_name;
}

char *symbol_get_type_name(type_t type)
{
    char *type_name;
    /*          12345678 */
    switch (type)
    {
    case TYPE_NONE:
        type_name = "NONE    ";
        break;
    case TYPE_INTEGER:
        type_name = "INTEGER ";
        break;
    default:
        type_name = "????????";
        break;
    }
    return type_name;
}

void symbol_dump(symbol_t *symbol)
{
    char *kind_name = symbol_get_kind_name(symbol->kind);
    char *type_name = symbol_get_type_name(symbol->type);
    char value[32];
    switch (symbol->type)
    {
    case TYPE_NONE:
        snprintf(value, 31, "?");
        break;
    case TYPE_INTEGER:
        snprintf(value, 31, "%d / %08x", symbol->value.i, symbol->value.i);
        break;
    default:
        snprintf(value, 31, "?");
        break;
    }
    fprintf(stderr,
            "SYMBOL: name=%s, type=%s, kind=%s, size=%ld, value=%s\n",
            symbol->name, kind_name, type_name, symbol->size, value);
}

/* EOF */
