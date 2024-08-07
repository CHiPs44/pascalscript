/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "ps_value.h"
#include "ps_symbol.h"

const struct s_ps_symbol_kind_name
{
    ps_symbol_kind kind;
    char *name;
} ps_symbol_kind_names[] = {
    {PS_SYMBOL_KIND_FREE, "FREE"},
    {PS_SYMBOL_KIND_AUTO, "AUTO"},
    {PS_SYMBOL_KIND_CONSTANT, "CONSTANT"},
    {PS_SYMBOL_KIND_VARIABLE, "VARIABLE"},
    {PS_SYMBOL_KIND_TYPE, "TYPE"},
};

char *ps_symbol_get_kind_name(ps_symbol_kind kind)
{
    static char name[16];
    for (int i = 0; i < sizeof(ps_symbol_kind_names) / sizeof(struct s_ps_symbol_kind_name); i += 1)
        if (ps_symbol_kind_names[i].kind == kind)
            return ps_symbol_kind_names[i].name;
    snprintf(name, 15, "? Unknown %d ?", kind);
    return name;
}

char *ps_symbol_get_scope_name(ps_scope scope)
{
    static char scope_name[8 + 1];
    if (scope==PS_SCOPE_SYSTEM)
        snprintf(scope_name, 7, "SYSTEM");
    if (scope == PS_SCOPE_GLOBAL)
        snprintf(scope_name, 7, "GLOBAL");
    else
        snprintf(scope_name, 7, PS_SCOPE_LOCAL_FORMAT, scope);
    return scope_name;
}

char *ps_symbol_dump(ps_symbol *symbol)
{
    static char buffer[256];
    snprintf(buffer, sizeof(buffer) - 1,
             "SYMBOL: name=%-*s, kind=%-16s, scope=%-8s, type=%-16s, value=%s",
             PS_IDENTIFIER_MAX,
             symbol->name,
             ps_symbol_get_kind_name(symbol->kind),
             ps_symbol_get_scope_name(symbol->scope),
             ps_value_get_type_name(symbol->value.type),
             ps_value_get_value(&symbol->value));
    return buffer;
}

void ps_symbol_debug(ps_symbol *symbol)
{
    fprintf(stderr, "DEBUG\t%s", ps_symbol_dump(symbol));
}

void ps_symbol_normalize_name(ps_symbol *symbol)
{
    char *name = symbol->name;
    while (*name)
        /* a-z => A-Z */
        if (*name >= 'a' && *name <= 'z')
            *name++ += ('A' - 'a');
}

// ps_symbol_hash_key ps_symbol_get_hash_key(ps_symbol *symbol)
// {
//     // cf.
//     ps_symbol_hash_key hash = 5381u;
//     char *name = symbol->name;
//     while (*name)
//     {
//         // 33 * x => 32 * x + x => x << 5 + x
//         hash = (hash << 5) + hash + *name;
//     }
//     return hash;
// }

/* EOF */
