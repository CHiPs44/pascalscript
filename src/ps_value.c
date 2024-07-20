/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "ps_error.h"
#include "ps_value.h"

static ps_error ps_value_errno = PS_ERROR_ZERO;

#define PS_VALUE_SET_XXX(PS_TYPE_X, ps_xxx, x)               \
    if (value == NULL)                                       \
    {                                                        \
        value = calloc(1, sizeof(ps_value));                 \
        if (value == NULL)                                   \
        {                                                    \
            ps_value_errno = PS_RUNTIME_ERROR_OUT_OF_MEMORY; \
            return NULL;                                     \
        }                                                    \
        value->type = PS_TYPE_X;                             \
        value->size = sizeof(ps_xxx);                        \
    }                                                        \
    else if (value->type != PS_TYPE_X)                       \
        return NULL;                                         \
    value->data.x = x;                                       \
    return value;

ps_value *ps_value_set_integer(ps_value *value, ps_integer i){
    PS_VALUE_SET_XXX(PS_TYPE_INTEGER, ps_integer, i)}

ps_value *ps_value_set_unsigned(ps_value *value, ps_unsigned u){
    PS_VALUE_SET_XXX(PS_TYPE_UNSIGNED, ps_unsigned, u)}

ps_value *ps_value_set_real(ps_value *value, ps_real r){
    PS_VALUE_SET_XXX(PS_TYPE_REAL, ps_real, r)}

ps_value *ps_value_set_boolean(ps_value *value, ps_boolean b){
    PS_VALUE_SET_XXX(PS_TYPE_BOOLEAN, ps_boolean, b)}

ps_value *ps_value_set_char(ps_value *value, ps_char c){
    PS_VALUE_SET_XXX(PS_TYPE_CHAR, ps_char, c)}

ps_value *ps_value_set_string(ps_value *value, char *s, ps_string_len max)
{
    ps_value_errno = PS_ERROR_NOT_IMPLEMENTED;
    return NULL;
    /*
    size_t len = strlen(s);
    if ((max > 0 && len > max) || (len > ps_string_max))
        return NULL;
    bool is_new = false;
    if (value == NULL)
    {
        is_new = true;
        value = calloc(1, sizeof(ps_value));
        if (value == NULL)
        {
            ps_value_errno = PS_RUNTIME_ERROR_OUT_OF_MEMORY;
            return NULL;
        }
        value->data.s->str = calloc(max + 1, sizeof(ps_char));
        value->type = PS_TYPE_STRING;
        value->size = sizeof(ps_string);
    }
    else if (value->type != PS_TYPE_STRING)
        return NULL;
    else
    {
        if (len > value->data.s->max)
        {
        }
    }
    = max;
    value->data.s->len = (ps_string_len)len;
    if (!is_new && value->data.s->str != NULL)
        free(value->data.s->str);
    value->data.s->str = calloc(data.s->len + 1);
    if (value->data->s.str == NULL)
    {
        if (is_new)
            free(value);
        return NULL;
    }
    strncpy(value->data.s.str, data.str, data.len);
    return value;
    */
}

ps_value *ps_value_set_pointer(ps_value *value, void *p){
    PS_VALUE_SET_XXX(PS_TYPE_POINTER, ps_pointer, p)}

ps_value *ps_value_set_enum(ps_value *value, ps_unsigned u){
    PS_VALUE_SET_XXX(PS_TYPE_ENUM, ps_unsigned, u)}

ps_value *ps_value_set_subrange(ps_value *value, ps_integer i)
{
    PS_VALUE_SET_XXX(PS_TYPE_SUBRANGE, ps_pointer, i)
}

const struct s_ps_type_name
{
    ps_type type;
    char *name;
} ps_type_names[] = {
    // clang-format off
    {PS_TYPE_NONE       , "NONE"    },
    {PS_TYPE_INTEGER    , "INTEGER" },
    {PS_TYPE_UNSIGNED   , "UNSIGNED"},
    {PS_TYPE_REAL       , "REAL"    },
    {PS_TYPE_BOOLEAN    , "BOOLEAN" },
    {PS_TYPE_CHAR       , "CHAR"    },
    {PS_TYPE_STRING     , "STRING"  },
    {PS_TYPE_POINTER    , "POINTER" },
    {PS_TYPE_ENUM       , "ENUM"    },
    {PS_TYPE_SUBRANGE   , "SUBRANGE"},
    {PS_TYPE_ARRAY      , "ARRAY"   },
    {PS_TYPE_RECORD     , "RECORD"  },
    // clang-format on
};

char *ps_value_get_type_name(ps_type type)
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
    static char buffer[512];
    switch (value->type)
    {
    case PS_TYPE_NONE:
        snprintf(buffer, sizeof(buffer) - 1, "[none]");
        break;
    case PS_TYPE_INTEGER:
        snprintf(buffer, sizeof(buffer) - 1, "%d", value->data.i);
        break;
    case PS_TYPE_UNSIGNED:
        snprintf(buffer, sizeof(buffer) - 1, "%u", value->data.u);
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
        snprintf(buffer, sizeof(buffer) - 1, "\"%s\"", value->data.s->str);
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
