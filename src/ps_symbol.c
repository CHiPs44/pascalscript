/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "ps_value.h"
#include "ps_symbol.h"

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

char *ps_symbol_get_scope_name(ps_symbol_scope scope)
{
    static char scope_name[PS_SYMBOL_SCOPE_NAME_SIZE];
    switch (scope)
    {
    // case PS_SYMBOL_SCOPE_NONE:
    //     snprintf(scope_name, PS_SYMBOL_SCOPE_NAME_LEN, "NONE");
    //     break;
    case PS_SYMBOL_SCOPE_SYSTEM:
        snprintf(scope_name, PS_SYMBOL_SCOPE_NAME_LEN, "SYSTEM");
        break;
    case PS_SYMBOL_SCOPE_GLOBAL:
        snprintf(scope_name, PS_SYMBOL_SCOPE_NAME_LEN, "GLOBAL");
        break;
    default:
        snprintf(scope_name, PS_SYMBOL_SCOPE_NAME_LEN, PS_SYMBOL_SCOPE_LOCAL_FORMAT, scope);
    }
    return scope_name;
}

const struct s_ps_symbol_kind_name
{
    ps_symbol_kind kind;
    char *name;
} ps_symbol_kind_names[] = {
    {PS_SYMBOL_KIND_AUTO, "AUTO"},
    {PS_SYMBOL_KIND_CONSTANT, "CONSTANT"},
    {PS_SYMBOL_KIND_VARIABLE, "VARIABLE"},
    {PS_SYMBOL_KIND_TYPE_DEFINITION, "TYPE"},
    {PS_SYMBOL_KIND_PROCEDURE, "PROCEDURE"},
    {PS_SYMBOL_KIND_FUNCTION, "FUNCTION"},
    {PS_SYMBOL_KIND_UNIT, "UNIT"},
    // ...
};

char *ps_symbol_get_kind_name(ps_symbol_kind kind)
{
    static char name[PS_SYMBOL_KIND_NAME_SIZE];
    bool found = false;
    for (int i = 0; i < sizeof(ps_symbol_kind_names) / sizeof(struct s_ps_symbol_kind_name); i += 1)
    {
        if (ps_symbol_kind_names[i].kind == kind)
        {
            snprintf(name, PS_SYMBOL_KIND_NAME_LEN, "%s", ps_symbol_kind_names[i].name);
            found = true;
            break;
        }
    }
    if (!found)
    {
        snprintf(name, PS_SYMBOL_KIND_NAME_LEN, "?UNKNOWN-%d?", kind);
    }
    return name;
}

char *ps_symbol_dump(ps_symbol *symbol)
{
    static char buffer[256];
    snprintf(buffer, sizeof(buffer) - 1,
             "SYMBOL: name=%-*s, scope=%-8s, kind=%-16s, type=%-16s, value=%s",
             PS_IDENTIFIER_LEN,
             symbol->name,
             ps_symbol_get_kind_name(symbol->kind),
             ps_symbol_get_scope_name(symbol->scope),
             ps_value_get_type_definition_name(symbol->value->type),
             ps_value_get_debug_value(&symbol->value));
    return buffer;
}

void ps_symbol_debug(ps_symbol *symbol)
{
    fprintf(stderr, "DEBUG\t%s", ps_symbol_dump(symbol));
}

/* EOF */
