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

ps_value_t *ps_value_string(ps_value_t *value, ps_char_t *data)
{
    value->type = PS_TYPE_STRING;
    value->size = ps_string_max + 1;
    strncpy(value->data.s, data, ps_string_max);
    return value;
}

ps_value_t *ps_value_real(ps_value_t *value, PS_REAL data)
{
    value->type = PS_TYPE_REAL;
    value->size = sizeof(PS_REAL);
    value->data.r = data;
    return value;
}

ps_value_t *ps_value_pointer(ps_value_t *value, void *data)
{
    value->type = PS_TYPE_POINTER;
    value->size = sizeof(void *);
    value->data.p = data;
    return value;
}

char *value_get_type_name(ps_type_t type)
{
    char *type_name;
    switch (type)
    {
    case PS_TYPE_ERROR:
        type_name = "ERROR";
        break;
    case PS_TYPE_NONE:
        type_name = "NONE";
        break;
    case PS_TYPE_INTEGER:
        type_name = "INTEGER";
        break;
    case PS_TYPE_UNSIGNED:
        type_name = "UNSIGNED";
        break;
    case PS_TYPE_BOOLEAN:
        type_name = "BOOLEAN";
        break;
    case PS_TYPE_REAL:
        type_name = "REAL";
        break;
    case PS_TYPE_CHAR:
        type_name = "CHAR";
        break;
    case PS_TYPE_STRING:
        type_name = "STRING";
        break;
    case PS_TYPE_POINTER:
        type_name = "POINTER";
        break;
    // default:
    //     type_name = "?";
    //     break;
    }
    return type_name;
}

char *value_get_value(ps_value_t *value)
{
    static char buffer[258 + 1];
    switch (value->type)
    {
    case PS_TYPE_NONE:
        snprintf(buffer, 258, "[none]");
        break;
    case PS_TYPE_INTEGER:
        snprintf(buffer, 258, "%d / 0x%08x", value->data.i, value->data.i);
        break;
    case PS_TYPE_UNSIGNED:
        snprintf(buffer, 258, "%u / 0x%08x", value->data.u, value->data.u);
        break;
    case PS_TYPE_BOOLEAN:
        snprintf(buffer, 258, "%s", value->data.b ? "[true]" : "[false]");
        break;
    case PS_TYPE_REAL:
        snprintf(buffer, 258, "%.20f", value->data.r);
        break;
    case PS_TYPE_CHAR:
        snprintf(buffer, 258, "'%c' / 0x%02x", value->data.c, value->data.c);
        break;
    case PS_TYPE_STRING:
        snprintf(buffer, 258, "\"%s\"", value->data.s);
        break;
    case PS_TYPE_POINTER:
        snprintf(buffer, 258, "%p", value->data.p);
        break;
    default:
        snprintf(buffer, 258, "[? %d ?]", value->type);
        break;
    }
    return buffer;
}

void value_dump(ps_value_t *value)
{
    char *type_name = value_get_type_name(value->type);
    char *buffer = value_get_value(value);
    fprintf(stderr,
            "VALUE: type=%s, size=%ld, value=%s\n",
            type_name, value->size, buffer);
}

/* EOF */
