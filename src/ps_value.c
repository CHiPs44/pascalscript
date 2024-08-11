/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "ps_error.h"
#include "ps_string.h"
#include "ps_value.h"

static ps_error ps_value_error = PS_ERROR_ZERO;

#define PS_VALUE_SET(PS_TYPE, x)                    \
    if (value == NULL)                                       \
    {                                                        \
        value = calloc(1, sizeof(ps_value));                 \
        if (value == NULL)                                   \
        {                                                    \
            ps_value_error = PS_RUNTIME_ERROR_OUT_OF_MEMORY; \
            return NULL;                                     \
        }                                                    \
    }                                                        \
    value->type = PS_TYPE;                                   \
    value->data.x = x;                                       \
    return value

ps_value *ps_value_set_integer(ps_value *value, ps_integer i)
{
    PS_VALUE_SET(PS_TYPE_INTEGER, i);
}

ps_value *ps_value_set_unsigned(ps_value *value, ps_unsigned u)
{
    PS_VALUE_SET(PS_TYPE_UNSIGNED, u);
}

ps_value *ps_value_set_real(ps_value *value, ps_real r)
{
    PS_VALUE_SET(PS_TYPE_REAL, r);
}

ps_value *ps_value_set_boolean(ps_value *value, ps_boolean b)
{
    PS_VALUE_SET(PS_TYPE_BOOLEAN, b);
}

ps_value *ps_value_set_char(ps_value *value, ps_char c)
{
    PS_VALUE_SET(PS_TYPE_CHAR, c);
}

ps_value *ps_value_set_enum(ps_value *value, ps_unsigned u)
{
    PS_VALUE_SET(PS_TYPE_ENUM, u);
}

ps_value *ps_value_set_subrange(ps_value *value, ps_integer i)
{
    PS_VALUE_SET(PS_TYPE_SUBRANGE, i);
}

ps_value *ps_value_set_string(ps_value *value, char *s, ps_string_len max)
{
    // ps_value_error = PS_ERROR_NOT_IMPLEMENTED;
    // return NULL;
    size_t len = strlen(s);
    if (max == 0)
        max = len;
    if (value == NULL && max == 0)
    {
        ps_value_error = PS_RUNTIME_ERROR_INVALID_PARAMETERS;
        return NULL;
    }
    bool is_new = false;
    if (value == NULL)
    {
        is_new = true;
        value = calloc(1, sizeof(ps_value));
        if (value == NULL)
        {
            ps_value_error = PS_RUNTIME_ERROR_OUT_OF_MEMORY;
            return NULL;
        }
    }
    value->type = PS_TYPE_STRING;
    value->size = sizeof(ps_string);
    if (max > 0 || is_new)
    {
        // (Re)allocate to new max length
        if (!is_new)
            ps_string_free(value->data.s);
        value->data.s = ps_string_create(max, s);
        if (value->data.s == NULL)
        {
            ps_value_error = errno == ENOMEM
                                 ? PS_RUNTIME_ERROR_OUT_OF_MEMORY
                                 : PS_RUNTIME_ERROR_OUT_OF_RANGE;
            if (is_new)
                free(value);
            return NULL;
        }
    }
    else
    {
        // Try to put new value in existing one
        ps_string *p = ps_string_set(value->data.s, s);
        if (p == NULL)
        {
            ps_value_error = errno == ENOMEM
                                 ? PS_RUNTIME_ERROR_OUT_OF_MEMORY
                                 : PS_RUNTIME_ERROR_OUT_OF_RANGE;
            if (is_new)
            {
                ps_string_free(value->data.s);
                free(value);
            }
            return NULL;
        }
    }
    return value;
}

ps_value *ps_value_set_pointer(ps_value *value, void *p)
{
    PS_VALUE_SET(PS_TYPE_POINTER, p);
}

const struct s_ps_type_name
{
    ps_value_type type;
    char *name;
} ps_type_names[] = {
    // clang-format off
    {PS_TYPE_NONE       , "NONE"    },
    {PS_TYPE_INTEGER    , "INTEGER" },
    {PS_TYPE_UNSIGNED   , "UNSIGNED"},
    {PS_TYPE_REAL       , "REAL"    },
    {PS_TYPE_BOOLEAN    , "BOOLEAN" },
    {PS_TYPE_CHAR       , "CHAR"    },
    {PS_TYPE_ENUM       , "ENUM"    },
    {PS_TYPE_SUBRANGE   , "SUBRANGE"},
    {PS_TYPE_SET        , "SET"     },
    {PS_TYPE_STRING     , "STRING"  },
    {PS_TYPE_DEFINITION , "TYPE_DEF"},
    {PS_TYPE_POINTER    , "POINTER" },
    {PS_TYPE_ARRAY      , "ARRAY"   },
    {PS_TYPE_RECORD     , "RECORD"  },
    {PS_TYPE_FILE       , "FILE"    },
    // clang-format on
};

char *ps_value_get_type_name(ps_value_type type)
{
    for (size_t i = 0; i < sizeof(ps_type_names) / sizeof(struct s_ps_type_name); i++)
    {
        if (type == ps_type_names[i].type)
            return ps_type_names[i].name;
    }
    return "UNKNOWN";
}

char *ps_value_get_value(ps_value *value)
{
    static char buffer[128];
    switch (value->type)
    {
    case PS_TYPE_NONE:
        snprintf(buffer, sizeof(buffer) - 1, "[none]");
        break;
    case PS_TYPE_INTEGER:
        snprintf(buffer, sizeof(buffer) - 1, "%d / 0x%x", value->data.i, value->data.i);
        break;
    case PS_TYPE_UNSIGNED:
        snprintf(buffer, sizeof(buffer) - 1, "%u / 0x%x", value->data.u, value->data.u);
        break;
    case PS_TYPE_REAL:
        snprintf(buffer, sizeof(buffer) - 1, "%G", value->data.r);
        break;
    case PS_TYPE_BOOLEAN:
        snprintf(buffer, sizeof(buffer) - 1, "%s", value->data.b ? "TRUE" : "FALSE");
        break;
    case PS_TYPE_CHAR:
        snprintf(buffer, sizeof(buffer) - 1, "'%c' / 0x%02x", value->data.c, value->data.c);
        break;
    case PS_TYPE_STRING:
        snprintf(buffer, sizeof(buffer) - 1, "\"%.*s\" (%d/%d)", 100, value->data.s->str, value->data.s->len, value->data.s->max);
        break;
    case PS_TYPE_POINTER:
        snprintf(buffer, sizeof(buffer) - 1, "%p", value->data.p);
        break;
    default:
        snprintf(buffer, sizeof(buffer) - 1, "[? %d ?]", value->type);
        break;
    }
    return buffer;
}

char *ps_value_dump(ps_value *value)
{
    static char buffer[512];
    snprintf(buffer, sizeof(buffer) - 1,
             "VALUE: type=%d/%s, size=%zu, value=%s",
             value->type,
             ps_value_get_type_name(value->type),
             value->size,
             ps_value_get_value(value));
    return buffer;
}

void ps_value_debug(ps_value *value, char *message)
{
    fprintf(stderr,
            "DEBUG\t%s%s\n",
            message == NULL ? "" : message,
            ps_value_dump(value));
}

/* EOF */
