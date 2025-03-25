/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

// #include "ps_string.h"
#include "ps_error.h"
#include "ps_symbol.h"
#include "ps_system.h"
#include "ps_type_definition.h"
#include "ps_value.h"

ps_value *ps_value_init(ps_type_definition *type, ps_value_data data)
{
    ps_value *value = calloc(1, sizeof(ps_value));
    if (value == NULL)
        return NULL;
    value->type = type;
    value->data = data;
    return value;
}

bool ps_value_is_scalar(ps_value *value)
{
    // /* /!\ based on enum order! */
    // return (value->type->base >= PS_TYPE_INTEGER && value->type->base <= PS_TYPE_SUBRANGE);
    return (value->type->base >= PS_TYPE_INTEGER && value->type->base <= PS_TYPE_CHAR);
}

bool ps_value_is_number(ps_value *value)
{
    /* /!\ based on enum order! */
    return (value->type->base >= PS_TYPE_REAL || value->type->base <= PS_TYPE_UNSIGNED);
}

#define PS_VALUE_SET(type_def, x)            \
    if (value == NULL)                       \
    {                                        \
        value = calloc(1, sizeof(ps_value)); \
        if (value == NULL)                   \
        {                                    \
            return NULL;                     \
        }                                    \
    }                                        \
    value->type = type_def;                  \
    value->data.x = x;                       \
    return value

ps_value *ps_value_set_integer(ps_value *value, ps_integer i)
{
    PS_VALUE_SET(ps_symbol_integer.value->type, i);
}

ps_value *ps_value_set_unsigned(ps_value *value, ps_unsigned u)
{
    PS_VALUE_SET(ps_symbol_unsigned.value->type, u);
}

ps_value *ps_value_set_real(ps_value *value, ps_real r)
{
    PS_VALUE_SET(ps_symbol_real.value->type, r);
}

ps_value *ps_value_set_boolean(ps_value *value, ps_boolean b)
{
    PS_VALUE_SET(ps_symbol_boolean.value->type, b);
}

ps_value *ps_value_set_char(ps_value *value, ps_char c)
{
    PS_VALUE_SET(ps_symbol_char.value->type, c);
}

// ps_value *ps_value_set_enum(ps_value *value, ps_unsigned e, ps_type_definition *type_def)
// {
//     if (PS_TYPE_ENUM != type_def->base)
//     {
//         ps_value_error = PS_RUNTIME_ERROR_TYPE_MISMATCH;
//         return NULL;
//     }
//     PS_VALUE_SET(type_def, e);
// }

// ps_value *ps_value_set_subrange(ps_value *value, ps_subrange g, ps_type_definition *type_def)
// {
//     if (PS_TYPE_ENUM != type_def->base)
//     {
//         ps_value_error = PS_RUNTIME_ERROR_TYPE_MISMATCH;
//         return NULL;
//     }
//     PS_VALUE_SET(type_def, g);
// }

// ps_value *ps_value_set_string(ps_value *value, ps_string *s)
// {
//     if (NULL == value)
//     {
//         return ps_value_new_string();
//     }
//     return s;
// }

// ps_string *ps_value_new_string(char *s, ps_string_len max, ps_string_len len)
// {
//     ps_value_error = PS_ERROR_NOT_IMPLEMENTED;
//     return NULL;
//     // if (max == 0)
//     //     max = len;
//     // if (value == NULL && max == 0)
//     // {
//     //     ps_value_error = PS_RUNTIME_ERROR_INVALID_PARAMETERS;
//     //     return NULL;
//     // }
//     // bool is_new = false;
//     // if (value == NULL)
//     // {
//     //     is_new = true;
//     //     value = calloc(1, sizeof(ps_value));
//     //     if (value == NULL)
//     //     {
//     //         ps_value_error = PS_RUNTIME_ERROR_OUT_OF_MEMORY;
//     //         return NULL;
//     //     }
//     // }
//     // value->type = PS_TYPE_STRING;
//     // // value->size = sizeof(ps_string);
//     // if (max > 0 || is_new)
//     // {
//     //     // (Re)allocate to new max length
//     //     if (!is_new)
//     //         ps_string_free(value->data.s);
//     //     value->data.s = ps_string_create(max, s);
//     //     if (value->data.s == NULL)
//     //     {
//     //         ps_value_error = errno == ENOMEM
//     //                              ? PS_RUNTIME_ERROR_OUT_OF_MEMORY
//     //                              : PS_RUNTIME_ERROR_OUT_OF_RANGE;
//     //         if (is_new)
//     //             free(value);
//     //         return NULL;
//     //     }
//     // }
//     // else
//     // {
//     //     // Try to put new value in existing one
//     //     ps_string *p = ps_string_set(value->data.s, s);
//     //     if (p == NULL)
//     //     {
//     //         ps_value_error = errno == ENOMEM
//     //                              ? PS_RUNTIME_ERROR_OUT_OF_MEMORY
//     //                              : PS_RUNTIME_ERROR_OUT_OF_RANGE;
//     //         if (is_new)
//     //         {
//     //             ps_string_free(value->data.s);
//     //             free(value);
//     //         }
//     //         return NULL;
//     //     }
//     // }
//     // return value;
// }

// ps_value *ps_value_set_pointer(ps_value *value, ps_pointer p, ps_type_definition *type_def)
// {
//     if (PS_TYPE_POINTER != type_def->base)
//     {
//         ps_value_error = PS_RUNTIME_ERROR_TYPE_MISMATCH;
//         return NULL;
//     }
//     PS_VALUE_SET(type_def, p);
// }

char *ps_value_get_debug_value(ps_value *value)
{
    static char buffer[128];
    // TODO? if (value->type->type!=value->type->base)
    switch (value->type->base)
    {
    case PS_TYPE_NONE:
        snprintf(buffer, sizeof(buffer) - 1, "[none]");
        break;
    case PS_TYPE_DEFINITION:
        snprintf(buffer, sizeof(buffer) - 1, "%s", ps_value_get_type_definition_name(value->type));
        break;
    case PS_TYPE_REAL:
        snprintf(buffer, sizeof(buffer) - 1, "%G", value->data.r);
        break;
    case PS_TYPE_INTEGER:
        snprintf(buffer, sizeof(buffer) - 1, "%d / 0x%x", value->data.i, value->data.i);
        break;
    case PS_TYPE_UNSIGNED:
        snprintf(buffer, sizeof(buffer) - 1, "%u / 0x%x", value->data.u, value->data.u);
        break;
    case PS_TYPE_BOOLEAN:
        snprintf(buffer, sizeof(buffer) - 1, "%s", value->data.b ? "TRUE" : "FALSE");
        break;
    case PS_TYPE_CHAR:
        snprintf(buffer, sizeof(buffer) - 1, "'%c' / 0x%02x", isprint(value->data.c) ? value->data.c : '.', value->data.c);
        break;
    // case PS_TYPE_STRING:
    //     snprintf(buffer, sizeof(buffer) - 1, "\"%.*s\" (%d/%d)", sizeof(buffer) - 20, value->data.s->str, value->data.s->len, value->data.s->max);
    //     break;
    // case PS_TYPE_POINTER:
    //     snprintf(buffer, sizeof(buffer) - 1, "%p", value->data.p);
    //     break;
    default:
        snprintf(buffer, sizeof(buffer) - 1, "[? %d ?]", value->type->base);
        break;
    }
    return buffer;
}

char *ps_value_dump(ps_value *value)
{
    static char buffer[512];
    char *type = value == NULL ? "NULL!" : value->type == NULL ? "TYPE!"
                                                               : "TODO1"; // ps_value_get_type_definition_name(value->type);
    char *data = value == NULL ? "NULL!" : ps_value_get_debug_value(value);
    snprintf(buffer, sizeof(buffer) - 1,
             "VALUE: type=%s, value=%s",
             type, data);
    return buffer;
}

void ps_value_debug(FILE *output, char *message, ps_value *value)
{
    if (output == NULL)
        output = stderr;
    fprintf(output,
            "DEBUG\t%s%s\n",
            message == NULL ? "" : message,
            ps_value_dump(value));
}

/* EOF */
