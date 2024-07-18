/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "ps_value.h"

ps_value *ps_value_make_integer(ps_value *value, ps_integer data)
{
    if (value == NULL)
    {
        value = calloc(1, sizeof(ps_value));
        if (value == NULL)
            return NULL;
    }
    value->type = PS_TYPE_INTEGER;
    value->size = sizeof(ps_integer);
    value->data.i = data;
    return value;
}

ps_value *ps_value_make_unsigned(ps_value *value, ps_unsigned data)
{
    if (value == NULL)
    {
        value = calloc(1, sizeof(ps_value));
        if (value == NULL)
            return NULL;
    }
    value->type = PS_TYPE_UNSIGNED;
    value->size = sizeof(ps_unsigned);
    value->data.u = data;
    return value;
}

ps_value *ps_value_make_real(ps_value *value, ps_real data)
{
    if (value == NULL)
    {
        value = calloc(1, sizeof(ps_value));
        if (value == NULL)
            return NULL;
    }
    value->type = PS_TYPE_REAL;
    value->size = sizeof(ps_real);
    value->data.r = data;
    return value;
}

ps_value *ps_value_make_boolean(ps_value *value, ps_boolean data)
{
    if (value == NULL)
    {
        value = calloc(1, sizeof(ps_value));
        if (value == NULL)
            return NULL;
    }
    value->type = PS_TYPE_BOOLEAN;
    value->size = sizeof(ps_boolean);
    value->data.b = data;
    return value;
}

ps_value *ps_value_make_char(ps_value *value, ps_char data)
{
    if (value == NULL)
    {
        value = calloc(1, sizeof(ps_value));
        if (value == NULL)
            return NULL;
    }
    value->type = PS_TYPE_CHAR;
    value->size = sizeof(ps_char);
    value->data.c = data;
    return value;
}

ps_value *ps_value_make_string(ps_value *value, ps_string data)
{
    bool allocated = false;
    if (value == NULL)
    {
        allocated = true;
        value = calloc(1, sizeof(ps_value));
        if (value == NULL)
            return NULL;
    }
    if (data.len > ps_string_max)
        return NULL;
    value->type = PS_TYPE_STRING;
    value->size = ps_string_max + 1;
    value->data.s.len = data.len;
    value->data.s.str = malloc(data.s.len + 1);
    if (value->data.s.str == NULL)
    {
        if (allocated)
            free(value);
        return NULL;
    }
    strncpy(value->data.s.str, data.str, data.len);
    return value;
}

ps_value *ps_value_make_pointer(ps_value *value, void *data)
{
    if (value == NULL)
    {
        value = calloc(1, sizeof(ps_value));
        if (value == NULL)
            return NULL;
    }
    value->type = PS_TYPE_POINTER;
    value->size = sizeof(void *);
    value->data.p = data;
    return value;
}

const struct ps_type_name
{
    ps_type type;
    char *name;
} ps_type_names[] = {
    {PS_TYPE_NONE, "NONE"},
    {PS_TYPE_INTEGER, "INTEGER"},
    {PS_TYPE_UNSIGNED, "UNSIGNED"},
    {PS_TYPE_REAL, "REAL"},
    {PS_TYPE_BOOLEAN, "BOOLEAN"},
    {PS_TYPE_CHAR, "CHAR"},
    {PS_TYPE_STRING, "STRING"},
    {PS_TYPE_POINTER, "POINTER"},
    {PS_TYPE_ENUM, "ENUM"},
    {PS_TYPE_SUBRANGE, "SUBRANGE"},
    {PS_TYPE_ARRAY, "ARRAY"},
    {PS_TYPE_RECORD, "RECORD"},
};

char *ps_value_get_type_name(ps_type type)
{
    for (size_t i = 0; i < sizeof(ps_type_names) / sizeof(struct ps_type_name); i++)
    {
        if (type == ps_type_names[i].type)
            return ps_type_names[i].name;
    }
    return "UNKNOWN";
}

char *ps_value_get_value(ps_value *value)
{
    static char buffer[512];
    switch (value->type)
    {
    case PS_TYPE_NONE:
        snprintf(buffer, sizeof(buffer) - 1, "[none]");
        break;
    case PS_TYPE_INTEGER:
        snprintf(buffer, sizeof(buffer) - 1, "%d / 0x%08x", value->data.i, value->data.i);
        break;
    case PS_TYPE_UNSIGNED:
        snprintf(buffer, sizeof(buffer) - 1, "%u / 0x%08x", value->data.u, value->data.u);
        break;
    case PS_TYPE_REAL:
        snprintf(buffer, sizeof(buffer) - 1, "%.20f", value->data.r);
        break;
    case PS_TYPE_BOOLEAN:
        snprintf(buffer, sizeof(buffer) - 1, "%s", value->data.b ? "[true]" : "[false]");
        break;
    case PS_TYPE_CHAR:
        snprintf(buffer, sizeof(buffer) - 1, "'%c' / 0x%02x", value->data.c, value->data.c);
        break;
    case PS_TYPE_STRING:
        snprintf(buffer, sizeof(buffer) - 1, "\"%s\"", value->data.s.str);
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
             "VALUE: type=%s, size=%ld, value=%s",
             ps_value_get_type_name(value->type),
             value->size,
             ps_value_get_value(value));
}

void ps_value_debug(ps_value *value)
{
    fprintf(stderr, "DEBUG\t%s\n", ps_value_dump(value));
}

/* EOF */
