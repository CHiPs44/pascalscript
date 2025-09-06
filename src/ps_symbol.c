/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_symbol.h"
#include "ps_value.h"

static uint32_t ps_symbol_auto_index = 0;

ps_symbol *ps_symbol_alloc(ps_symbol_kind kind, ps_identifier *name, ps_value *value)
{
    ps_symbol *symbol = (ps_symbol *)calloc(1, sizeof(ps_symbol));
    if (symbol == NULL)
        return NULL;
    symbol->kind = kind;
    if (name != NULL)
        memcpy(&symbol->name, name, PS_IDENTIFIER_SIZE);
    else
    {
        memset(&symbol->name, 0, PS_IDENTIFIER_SIZE);
        snprintf((char *)&symbol->name, PS_IDENTIFIER_LEN, PS_SYMBOL_AUTO_FORMAT, ps_symbol_auto_index++);
    }
    symbol->allocated = true;
    symbol->value = value;
    return symbol;
}

ps_symbol *ps_symbol_free(ps_symbol *symbol)
{
    if (symbol != NULL)
    {
        // free only allocated symbols
        if (symbol->allocated)
        {
            symbol->value = ps_value_free(symbol->value);
            free(symbol);
        }
    }
    return NULL;
}

void ps_symbol_normalize_name(ps_symbol *symbol)
{
    char *name = symbol->name;
    while (*name)
        /* a-z => A-Z */
        if (*name >= 'a' && *name <= 'z')
            *name++ += ('A' - 'a');
}

const struct s_ps_symbol_kind_name
{
    ps_symbol_kind kind;
    char *name;
} ps_symbol_kind_names[] = {
    // clang-format off
    //                                123456789
    {PS_SYMBOL_KIND_AUTO           , "AUTO"     },
    {PS_SYMBOL_KIND_TYPE_DEFINITION, "TYPE"     },
    {PS_SYMBOL_KIND_PROGRAM        , "PROGRAM"  },
    {PS_SYMBOL_KIND_UNIT           , "UNIT"     },
    {PS_SYMBOL_KIND_CONSTANT       , "CONSTANT" },
    {PS_SYMBOL_KIND_VARIABLE       , "VARIABLE" },
    {PS_SYMBOL_KIND_PROCEDURE      , "PROCEDURE"},
    {PS_SYMBOL_KIND_FUNCTION       , "FUNCTION" },
    // ...
    // clang-format on
};

char *ps_symbol_get_kind_name(ps_symbol_kind kind)
{
    static char kind_name[PS_IDENTIFIER_SIZE];
    for (int i = 0; i < sizeof(ps_symbol_kind_names) / sizeof(struct s_ps_symbol_kind_name); i += 1)
    {
        if (ps_symbol_kind_names[i].kind == kind)
        {
            snprintf(kind_name, PS_IDENTIFIER_LEN, "%s", ps_symbol_kind_names[i].name);
            return kind_name;
        }
    }
    snprintf(kind_name, PS_IDENTIFIER_LEN, "?UNKNOWN-%d?", kind);
    return kind_name;
}

char *ps_symbol_dump_value(ps_symbol *symbol)
{
    static char buffer[256];
    snprintf(buffer, sizeof(buffer) - 1, "SYMBOL: name=%-*s, kind=%-16s, type=%-16s, value=%s", PS_IDENTIFIER_LEN + 1,
             symbol == NULL ? "NULL!" : symbol->name, symbol == NULL ? "NULL!" : ps_symbol_get_kind_name(symbol->kind),
             symbol == NULL ? "NULL!" : ps_type_definition_get_name(symbol->value->type->value->data.t),
             symbol == NULL ? "NULL!" : ps_value_get_debug_value(symbol->value));
    return buffer;
}

void ps_symbol_debug(FILE *output, char *message, ps_symbol *symbol)
{
    if (output == NULL)
        output = stderr;
    fprintf(output, "DEBUG\t%s%s\n", message, ps_symbol_dump_value(symbol));
}

/* EOF */
