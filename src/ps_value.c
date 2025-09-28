/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "ps_error.h"
#include "ps_string.h"
#include "ps_symbol.h"
#include "ps_system.h"
#include "ps_type_definition.h"
#include "ps_value.h"

ps_value *ps_value_alloc(ps_symbol *type, ps_value_data data)
{
    ps_value *value = calloc(1, sizeof(ps_value));
    if (value == NULL)
        return NULL;
    value->type = type;
    value->data = data;
    value->allocated = true;
    return value;
}

ps_value *ps_value_free(ps_value *value)
{
    if (value == NULL || !value->allocated)
        return NULL;
    free(value);
    return NULL;
}

#define PS_VALUE_SET(__TYPE__, __X__)                                                                                  \
    if (value == NULL)                                                                                                 \
    {                                                                                                                  \
        value = ps_value_alloc(&__TYPE__, (ps_value_data){.__X__ = __X__});                                            \
        if (value == NULL)                                                                                             \
            return NULL;                                                                                               \
    }                                                                                                                  \
    else                                                                                                               \
    {                                                                                                                  \
        value->type = &__TYPE__;                                                                                       \
        value->data.__X__ = __X__;                                                                                     \
    }                                                                                                                  \
    return value;

ps_value *ps_value_set_integer(ps_value *value, ps_integer i)
{
    PS_VALUE_SET(ps_system_integer, i);
}

ps_value *ps_value_set_unsigned(ps_value *value, ps_unsigned u)
{
    PS_VALUE_SET(ps_system_unsigned, u);
}

ps_value *ps_value_set_real(ps_value *value, ps_real r)
{
    PS_VALUE_SET(ps_system_real, r);
}

ps_value *ps_value_set_boolean(ps_value *value, ps_boolean b)
{
    PS_VALUE_SET(ps_system_boolean, b);
}

ps_value *ps_value_set_char(ps_value *value, ps_char c)
{
    PS_VALUE_SET(ps_system_char, c);
}

// ps_value *ps_value_set_enum(ps_value *value, ps_unsigned e, ps_type_definition *type_def)
// {
//     if (PS_TYPE_ENUM != type_def->base)
//     {
//         ps_value_error = PS_ERROR_TYPE_MISMATCH;
//         return NULL;
//     }
//     PS_VALUE_SET(type_def, e);
// }

// ps_value *ps_value_set_subrange(ps_value *value, ps_subrange g, ps_type_definition *type_def)
// {
//     if (PS_TYPE_ENUM != type_def->base)
//     {
//         ps_value_error = PS_ERROR_TYPE_MISMATCH;
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
//     //     ps_value_error = PS_ERROR_INVALID_PARAMETERS;
//     //     return NULL;
//     // }
//     // bool is_new = false;
//     // if (value == NULL)
//     // {
//     //     is_new = true;
//     //     value = calloc(1, sizeof(ps_value));
//     //     if (value == NULL)
//     //     {
//     //         ps_value_error = PS_ERROR_OUT_OF_MEMORY;
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
//     //                              ? PS_ERROR_OUT_OF_MEMORY
//     //                              : PS_ERROR_OUT_OF_RANGE;
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
//     //                              ? PS_ERROR_OUT_OF_MEMORY
//     //                              : PS_ERROR_OUT_OF_RANGE;
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
//         ps_value_error = PS_ERROR_TYPE_MISMATCH;
//         return NULL;
//     }
//     PS_VALUE_SET(type_def, p);
// }

char *ps_value_to_string(ps_value *value, bool debug)
{
    static char buffer[PS_STRING_MAX_LEN + 1];
    ps_executable *executable;
    if (value == NULL)
    {
        if (debug)
        {
            snprintf(buffer, sizeof(buffer) - 1, "NULL VALUE");
            return buffer;
        }
        return NULL;
    }
    if (value->type == NULL)
    {
        if (debug)
        {
            snprintf(buffer, sizeof(buffer) - 1, "NULL __TYPE__");
            return buffer;
        }
        return NULL;
    }
    if (value->type->value == NULL)
    {
        if (debug)
        {
            snprintf(buffer, sizeof(buffer) - 1, "NULL __TYPE__ VALUE");
            return buffer;
        }
        return NULL;
    }
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_NONE:
        if (debug)
            snprintf(buffer, sizeof(buffer) - 1, "[none]");
        else
            return NULL;
        break;
    case PS_TYPE_DEFINITION:
        if (debug)
            snprintf(buffer, sizeof(buffer) - 1, "%s", value->type->name);
        else
            return NULL;
        break;
    case PS_TYPE_REAL:
        snprintf(buffer, sizeof(buffer) - 1, "%" PS_REAL_FMT, value->data.r);
        break;
    case PS_TYPE_INTEGER:
        if (debug)
            snprintf(buffer, sizeof(buffer) - 1, "%" PS_INTEGER_FMT_10 " / 0x%" PS_UNSIGNED_FMT_16, value->data.i,
                     value->data.i);
        else
            snprintf(buffer, sizeof(buffer) - 1, "%" PS_INTEGER_FMT_10, value->data.i);
        break;
    case PS_TYPE_UNSIGNED:
        if (debug)
            snprintf(buffer, sizeof(buffer) - 1, "%" PS_UNSIGNED_FMT_10 " / 0x%" PS_UNSIGNED_FMT_16, value->data.u,
                     value->data.u);
        else
            snprintf(buffer, sizeof(buffer) - 1, "%" PS_UNSIGNED_FMT_10, value->data.u);
        break;
    case PS_TYPE_BOOLEAN:
        snprintf(buffer, sizeof(buffer) - 1, "%s", value->data.b ? "TRUE" : "FALSE");
        break;
    case PS_TYPE_CHAR:
        if (debug)
            snprintf(buffer, sizeof(buffer) - 1, "'%c' / %03d / 0x%02x", isprint(value->data.c) ? value->data.c : '.',
                     value->data.c, value->data.c);
        else
            snprintf(buffer, sizeof(buffer) - 1, "%c", value->data.c);
        break;
    case PS_TYPE_STRING:
        if (debug)
            snprintf(buffer, sizeof(buffer) - 1, "%03d/%03d \"%.*s\"", value->data.s->len, value->data.s->max,
                     PS_IDENTIFIER_LEN - 10, value->data.s->str);
        else
        {
            memset(buffer, 0, sizeof(buffer));
            memcpy(buffer, value->data.s->str, value->data.s->len);
        }
        break;
    case PS_TYPE_EXECUTABLE:
        executable = value->data.x;
        if (executable == NULL)
            snprintf(buffer, sizeof(buffer) - 1, "NULL!");
        else if (executable->system != NULL)
        {
            snprintf(buffer, sizeof(buffer) - 1, "SYSTEM@%p", executable->system);
        }
        else
        {
            snprintf(buffer, sizeof(buffer) - 1, "%s@L:%05d/C:%03d",
                     executable->signature->result_type == NULL || executable->signature->result_type == &ps_system_none
                         ? "PROCEDURE"
                         : "FUNCTION",
                     executable->line + 1, executable->column + 1);
        }
        break;
    case PS_TYPE_POINTER:
        snprintf(buffer, sizeof(buffer) - 1, "%p", value->data.p);
        break;
    default:
        if (debug)
            snprintf(buffer, sizeof(buffer) - 1, "[? %s ?]", value->type->name);
        else
            return NULL;
        break;
    }
    return buffer;
}

char *ps_value_get_display_string(ps_value *value)
{
    return ps_value_to_string(value, false);
}

char *ps_value_get_debug_string(ps_value *value)
{
    return ps_value_to_string(value, true);
}

char *ps_value_dump(ps_value *value)
{
    static char buffer[512];
    char *type = value == NULL ? "NULL!" : value->type == NULL ? "__TYPE__!" : value->type->name;
    char *data = value == NULL ? "NULL!" : ps_value_get_debug_string(value);
    snprintf(buffer, sizeof(buffer) - 1, "VALUE: type=%s, value=%s", type, data);
    return buffer;
}

void ps_value_debug(FILE *output, char *message, ps_value *value)
{
    if (output == NULL)
        output = stderr;
    fprintf(output, "DEBUG\t%s%s\n", message == NULL ? "" : message, ps_value_dump(value));
}

/* EOF */
