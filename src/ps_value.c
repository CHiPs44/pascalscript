/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "ps_value.h"

ps_value_t *ps_value_integer(ps_value_t *value, ps_integer_t data)
{
    value->type = PS_TYPE_INTEGER;
    value->size = sizeof(ps_integer_t);
    value->data.i = data;
    return value;
}

ps_value_t *ps_value_unsigned(ps_value_t *value, ps_unsigned_t data)
{
    value->type = PS_TYPE_UNSIGNED;
    value->size = sizeof(ps_unsigned_t);
    value->data.u = data;
    return value;
}

ps_value_t *ps_value_real(ps_value_t *value, ps_real_t data)
{
    value->type = PS_TYPE_REAL;
    value->size = sizeof(ps_real_t);
    value->data.r = data;
    return value;
}

ps_value_t *ps_value_boolean(ps_value_t *value, ps_boolean_t data)
{
    value->type = PS_TYPE_BOOLEAN;
    value->size = sizeof(ps_boolean_t);
    value->data.b = data;
    return value;
}

ps_value_t *ps_value_char(ps_value_t *value, ps_char_t data)
{
    value->type = PS_TYPE_CHAR;
    value->size = sizeof(ps_char_t);
    value->data.c = data;
    return value;
}

ps_value_t *ps_value_string(ps_value_t *value, ps_string_t data)
{
    value->type = PS_TYPE_STRING;
    value->size = ps_string_max + 1;
    if (data.len > ps_string_max)
        return NULL;
    value->data.s.len = data.len;
    strncpy(value->data.s.str, data.str, data.len);
    return value;
}

ps_value_t *ps_value_pointer(ps_value_t *value, void *data)
{
    value->type = PS_TYPE_POINTER;
    value->size = sizeof(void *);
    value->data.p = data;
    return value;
}

const char *ps_type_names[] = {"NONE", "INTEGER", "UNSIGNED", "REAL", "BOOLEAN", "CHAR", "STRING", "POINTER"};

char *ps_value_get_type_name(ps_type_t type)
{
    if (type >= PS_TYPE_NONE && type <= PS_TYPE_POINTER)
        return ps_type_names[type];
    return "UNKNOWN";
}

char *ps_value_get_value(ps_value_t *value)
{
    static char buffer[256 + 2 + 1];
    switch (value->type)
    {
    case PS_TYPE_NONE:
        snprintf(buffer, 256 + 2, "[none]");
        break;
    case PS_TYPE_INTEGER:
        snprintf(buffer, 256 + 2, "%d / 0x%08x", value->data.i, value->data.i);
        break;
    case PS_TYPE_UNSIGNED:
        snprintf(buffer, 256 + 2, "%u / 0x%08x", value->data.u, value->data.u);
        break;
    case PS_TYPE_REAL:
        snprintf(buffer, 256 + 2, "%.20f", value->data.r);
        break;
    case PS_TYPE_BOOLEAN:
        snprintf(buffer, 256 + 2, "%s", value->data.b ? "[true]" : "[false]");
        break;
    case PS_TYPE_CHAR:
        snprintf(buffer, 256 + 2, "'%c' / 0x%02x", value->data.c, value->data.c);
        break;
    case PS_TYPE_STRING:
        snprintf(buffer, 256 + 2, "\"%s\"", value->data.s.str);
        break;
    case PS_TYPE_POINTER:
        snprintf(buffer, 256 + 2, "%p", value->data.p);
        break;
    default:
        snprintf(buffer, 256 + 2, "[? %d ?]", value->type);
        break;
    }
    return buffer;
}

void ps_value_dump(ps_value_t *value)
{
    char *type_name = ps_value_get_type_name(value->type);
    char *buffer = ps_value_get_value(value);
    fprintf(stderr,
            "VALUE: type=%s, size=%ld, value=%s\n",
            type_name, value->size, buffer);
}

/* EOF */
