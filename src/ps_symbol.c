/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_value.h"
#include "ps_symbol.h"

ps_symbol *ps_symbol_init(ps_symbol_scope scope, ps_symbol_kind kind, ps_identifier *name, ps_value *value)
{
    ps_symbol *symbol = (ps_symbol *)calloc(1, sizeof(ps_symbol));
    if (symbol == NULL)
        return NULL;
    symbol->scope = scope;
    symbol->kind = kind;
    if (name != NULL)
        memcpy(&symbol->name, name, PS_IDENTIFIER_LEN + 1);
    symbol->value = value;
    // fprintf(stderr, "ps_symbol_init: %s\n", name == NULL ? "NULL" : (char *)name);
    return symbol;
}

void ps_symbol_free(ps_symbol *symbol)
{
    free(symbol);
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

bool ps_symbol_scope_is_unit(ps_symbol_scope scope)
{
    return scope >= PS_SYMBOL_SCOPE_UNIT && scope < PS_SYMBOL_SCOPE_LOCAL;
}

bool ps_symbol_scope_is_local(ps_symbol_scope scope)
{
    return scope >= PS_SYMBOL_SCOPE_LOCAL;
}

char *ps_symbol_get_scope_name(ps_symbol_scope scope)
{
    static char scope_name[PS_SYMBOL_SCOPE_NAME_SIZE];
    if (scope == PS_SYMBOL_SCOPE_SYSTEM)
        snprintf(scope_name, PS_SYMBOL_SCOPE_NAME_LEN, "SYSTEM");
    else if (scope == PS_SYMBOL_SCOPE_GLOBAL)
        snprintf(scope_name, PS_SYMBOL_SCOPE_NAME_LEN, "GLOBAL");
    else if (scope >= PS_SYMBOL_SCOPE_LOCAL && scope < PS_SYMBOL_SCOPE_UNIT)
        snprintf(scope_name, PS_SYMBOL_SCOPE_NAME_LEN, "L%06d" /*PS_SYMBOL_SCOPE_LOCAL_FORMAT*/, scope);
    else
        snprintf(scope_name, PS_SYMBOL_SCOPE_NAME_LEN, "U%06d" /*PS_SYMBOL_SCOPE_UNIT_FORMAT*/, scope);
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
    {PS_SYMBOL_KIND_PROGRAM, "PROGRAM"},
    {PS_SYMBOL_KIND_UNIT, "UNIT"},
    // ...
};

char *ps_symbol_get_kind_name(ps_symbol_kind kind)
{
    static char kind_name[PS_IDENTIFIER_SIZE];
    bool found = false;
    for (int i = 0; i < sizeof(ps_symbol_kind_names) / sizeof(struct s_ps_symbol_kind_name); i += 1)
    {
        if (ps_symbol_kind_names[i].kind == kind)
        {
            snprintf(kind_name, PS_IDENTIFIER_LEN, "%s", ps_symbol_kind_names[i].name);
            found = true;
            break;
        }
    }
    if (!found)
    {
        snprintf(kind_name, PS_IDENTIFIER_LEN, "?UNKNOWN-%d?", kind);
    }
    return kind_name;
}

char *ps_symbol_dump_header()
{
    return "TODO?";
}

char *ps_symbol_dump_value(ps_symbol *symbol)
{
    static char buffer[256];
    snprintf(buffer, sizeof(buffer) - 1,
             "SYMBOL: name=%-*s, scope=%-8s, kind=%-16s, type=%-16s, value=%s",
             PS_IDENTIFIER_LEN,
             symbol->name,
             ps_symbol_get_scope_name(symbol->scope),
             ps_symbol_get_kind_name(symbol->kind),
             ps_value_get_type_definition_name(symbol->value->type),
             ps_value_get_debug_value(symbol->value));
    return buffer;
}

void ps_symbol_debug(ps_symbol *symbol)
{
    fprintf(stderr, "DEBUG\t%s", ps_symbol_dump_value(symbol));
}

/* EOF */
