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
void ps_symbol_normalize_name(char *name)
{
    while (*name)
        /* a-z => A-Z */
        if (*name >= 'a' && *name <= 'z')
            *name++ -= ('a' - 'A');
}

const char *ps_symbol_kind_names[] = {
    "FREE", "AUTO", "CONSTANT",
    "VARIABLE", "PROCEDURE", "FUNCTION",
    "TYPE", "POINTER", "SET", "RECORD"};

char *ps_symbol_get_kind_name(ps_kind kind)
{
    static char kind_name[16];
    if (kind >= KIND_FREE && kind <= KIND_RECORD)
        return ps_symbol_kind_names[kind];
    snprintf(kind_name, 15, "? Unknown %d ?", kind);
    return kind_name;
}

char *symbol_get_scope_name(ps_scope scope)
{
    static char scope_name[8 + 1];
    if (scope == PS_SCOPE_GLOBAL)
        snprintf(scope_name, 15, "GLOBAL");
    else
        snprintf(scope_name, 15, "LOCAL%02d", scope);
    return scope_name;
}

char *ps_symbol_dump(ps_symbol *symbol)
{
    static char buffer[512];
    snprintf(buffer, sizeof(buffer)-1,
             "SYMBOL: name=%-32s, kind=%-16s, scope=%-8s, type=%-16s, size=%ld, value=%s",
             symbol->name, 
             ps_symbol_get_kind_name(symbol->kind), 
             symbol_get_scope_name(symbol->scope), 
             ps_value_get_type_name(symbol->value.type), 
             symbol->value.size, 
             ps_value_get_value(&symbol->value));
    return buffer;
}

void ps_symbol_debug(ps_symbol *symbol)
{
    fprintf(stderr, "DEBUG\t%s", ps_symbol_dump(symbol));
}

/* EOF */
