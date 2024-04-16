/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "ps_symbol.h"

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

char *symbol_get_type_name(type_t type)
{
    char *type_name;
    switch (type)
    {
    case TYPE_NONE:
        type_name = "NONE";
        break;
    case TYPE_INTEGER:
        type_name = "INTEGER";
        break;
    case TYPE_UNSIGNED_INTEGER:
        type_name = "UNSIGNED";
        break;
    case TYPE_REAL:
        type_name = "REAL";
        break;
    case TYPE_CHAR:
        type_name = "CHAR";
        break;
    case TYPE_STRING:
        type_name = "STRING";
        break;
    case TYPE_POINTER:
        type_name = "POINTER";
        break;
    default:
        type_name = "?";
        break;
    }
    return type_name;
}

char *symbol_get_value(symbol_t *symbol)
{
    static char value[256 + 1];
    switch (symbol->type)
    {
    case TYPE_NONE:
        snprintf(value, 256, "?");
        break;
    case TYPE_INTEGER:
        snprintf(value, 256, "%d / %08x", symbol->value.i, symbol->value.i);
        break;
    case TYPE_UNSIGNED_INTEGER:
        snprintf(value, 256, "%ud / %08x", symbol->value.u, symbol->value.u);
        break;
    case TYPE_REAL:
        snprintf(value, 256, "%f", symbol->value.r);
        break;
    case TYPE_CHAR:
        snprintf(value, 256, "%c / %02x", symbol->value.c, symbol->value.c);
        break;
    case TYPE_STRING:
        snprintf(value, 256, "%s", symbol->value.s);
        break;
    default:
        snprintf(value, 256, "? %d ?", symbol->type);
        break;
    }
    return value;
}

void symbol_dump(symbol_t *symbol)
{
    char *kind_name = symbol_get_kind_name(symbol->kind);
    char *type_name = symbol_get_type_name(symbol->type);
    char *value = symbol_get_value(symbol);
    fprintf(stderr,
            "SYMBOL: name=%s, type=%s, kind=%s, size=%ld, value=%s\n",
            symbol->name, kind_name, type_name, symbol->size, value);
}

/* EOF */
