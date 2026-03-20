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
#include "ps_memory.h"
#include "ps_string.h"
#include "ps_symbol.h"
#include "ps_system.h"
#include "ps_type_definition.h"
#include "ps_value.h"

ps_value *ps_value_alloc(ps_symbol *type, ps_value_data data)
{
    ps_value *value = ps_memory_malloc(PS_MEMORY_VALUE, sizeof(ps_value));
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
    if (value->type != NULL && value->type->value != NULL && value->type->value->data.t != NULL &&
        value->type->value->data.t->base == PS_TYPE_EXECUTABLE)
    {
        ps_executable_free(value->data.x);
    }
    ps_memory_free(PS_MEMORY_VALUE, value);
    return NULL;
}

bool ps_value_is_scalar(const ps_value *value)
{
    if (value != NULL && value->type != NULL && value->type->value != NULL && value->type->value->data.t != NULL)
    {
        ps_value_type type = value->type->value->data.t->type;
        ps_value_type base = value->type->value->data.t->base;
        if (type == PS_TYPE_UNSIGNED || type == PS_TYPE_INTEGER ||
            (type == PS_TYPE_SUBRANGE && base == PS_TYPE_UNSIGNED) ||
            (type == PS_TYPE_SUBRANGE && base == PS_TYPE_INTEGER))
            return true;
    }
    return false;
}

bool ps_value_is_ordinal(const ps_value *value)
{
    if (value != NULL && value->type != NULL && value->type->value != NULL && value->type->value->data.t != NULL)
    {
        ps_value_type type = value->type->value->data.t->type;
        if (type == PS_TYPE_BOOLEAN || type == PS_TYPE_CHAR || value->type->value->data.t->type == PS_TYPE_ENUM)
            return true;
    }
    return false;
}

bool ps_value_is_number(const ps_value *value)
{
    if (value != NULL && value->type != NULL && value->type->value != NULL && value->type->value->data.t != NULL)
    {
        ps_value_type type = value->type->value->data.t->type;
        ps_value_type base = value->type->value->data.t->base;
        return (type == PS_TYPE_UNSIGNED || type == PS_TYPE_INTEGER || type == PS_TYPE_REAL ||
                (type == PS_TYPE_SUBRANGE && base == PS_TYPE_UNSIGNED) ||
                (type == PS_TYPE_SUBRANGE && base == PS_TYPE_INTEGER));
    }
    return false;
}

bool ps_value_is_real(const ps_value *value)
{
    if (value != NULL && value->type != NULL && value->type->value != NULL && value->type->value->data.t != NULL)
    {
        ps_value_type type = value->type->value->data.t->type;
        ps_value_type base = value->type->value->data.t->base;
        if (type == PS_TYPE_REAL || base == PS_TYPE_REAL)
            return true;
    }
    return false;
}

bool ps_value_is_string(const ps_value *value)
{
    if (value != NULL && value->type != NULL && value->type->value != NULL && value->type->value->data.t != NULL)
    {
        ps_value_type type = value->type->value->data.t->type;
        ps_value_type base = value->type->value->data.t->base;
        if (type == PS_TYPE_STRING || base == PS_TYPE_STRING)
            return true;
    }
    return false;
}

bool ps_value_is_array(const ps_value *value)
{
    if (value != NULL && value->type != NULL && value->type->value != NULL && value->type->value->data.t != NULL)
    {
        ps_value_type type = value->type->value->data.t->type;
        ps_value_type base = value->type->value->data.t->base;
        if (type == PS_TYPE_ARRAY && base == PS_TYPE_ARRAY)
            return true;
    }
    return false;
}

ps_value_type ps_value_get_type(const ps_value *value)
{
    ps_value_type value_type = PS_TYPE_NONE;
    if (value != NULL && value->type != NULL && value->type->value != NULL && value->type->value->data.t != NULL)
    {
        value_type = value->type->value->data.t->type;
    }
    return value_type;
}

ps_value_type ps_value_get_base(const ps_value *value)
{
    ps_value_type value_type = PS_TYPE_NONE;
    if (value != NULL && value->type != NULL && value->type->value != NULL && value->type->value->data.t != NULL)
    {
        value_type = value->type->value->data.t->base;
    }
    return value_type;
}

ps_error ps_value_copy(const ps_value *from, ps_value *to, bool range_check)
{
    // If destination type is NONE, set it to source type
    if (to->type == NULL || to->type == &ps_system_none || ps_value_get_base(to) == PS_TYPE_NONE)
        to->type = from->type;
    // Same type, just copy value
    if (from->type == to->type)
    {
        to->data = from->data;
        return PS_ERROR_NONE;
    }
    // Char => Char? (subrange)
    if (ps_value_get_base(from) == PS_TYPE_CHAR && ps_value_get_base(to) == PS_TYPE_CHAR)
    {
        if (range_check && ps_value_get_type(to) == PS_TYPE_SUBRANGE &&
            (from->data.c < to->type->value->data.t->def.g.c.min ||
             from->data.c > to->type->value->data.t->def.g.c.max))
            return PS_ERROR_OUT_OF_RANGE;
        to->data.c = from->data.c;
        return PS_ERROR_NONE;
    }
    // Integer => Integer? (subrange)
    if (ps_value_get_base(from) == PS_TYPE_INTEGER && ps_value_get_base(to) == PS_TYPE_INTEGER)
    {
        if (range_check && ps_value_get_type(to) == PS_TYPE_SUBRANGE &&
            (from->data.i < to->type->value->data.t->def.g.i.min ||
             from->data.i > to->type->value->data.t->def.g.i.max))
            return PS_ERROR_OUT_OF_RANGE;
        to->data.i = from->data.i;
        return PS_ERROR_NONE;
    }
    // Unsigned => Unsigned? (subrange)
    if (ps_value_get_base(from) == PS_TYPE_UNSIGNED && ps_value_get_base(to) == PS_TYPE_UNSIGNED)
    {
        if (range_check && ps_value_get_type(to) == PS_TYPE_SUBRANGE &&
            (from->data.u < to->type->value->data.t->def.g.u.min ||
             from->data.u > to->type->value->data.t->def.g.u.max))
            return PS_ERROR_OUT_OF_RANGE;
        to->data.u = from->data.u;
        return PS_ERROR_NONE;
    }
    // Integer => Unsigned?
    if (ps_value_get_base(from) == PS_TYPE_INTEGER && ps_value_get_base(to) == PS_TYPE_UNSIGNED)
    {
        if (range_check && from->data.i < 0)
            return PS_ERROR_OUT_OF_RANGE;
        if (range_check && ps_value_get_type(to) == PS_TYPE_SUBRANGE &&
            ((ps_unsigned)from->data.i < to->type->value->data.t->def.g.u.min ||
             (ps_unsigned)from->data.i > to->type->value->data.t->def.g.u.max))
            return PS_ERROR_OUT_OF_RANGE;
        to->data.u = (ps_unsigned)from->data.i;
        return PS_ERROR_NONE;
    }
    // Unsigned => Integer?
    if (ps_value_get_base(from) == PS_TYPE_UNSIGNED && ps_value_get_base(to) == PS_TYPE_INTEGER)
    {
        if (range_check && from->data.u > PS_INTEGER_MAX)
            return PS_ERROR_OUT_OF_RANGE;
        if (range_check && ps_value_get_type(to) == PS_TYPE_SUBRANGE &&
            ((ps_integer)from->data.u < to->type->value->data.t->def.g.i.min ||
             (ps_integer)from->data.u > to->type->value->data.t->def.g.i.max))
            return PS_ERROR_OUT_OF_RANGE;
        to->data.i = (ps_integer)from->data.u;
        return PS_ERROR_NONE;
    }
    // Integer => Real? (no range check needed, as real can hold all integer values)
    if (ps_value_get_base(from) == PS_TYPE_INTEGER && ps_value_get_base(to) == PS_TYPE_REAL)
    {
        to->data.r = (ps_real)from->data.i;
        return PS_ERROR_NONE;
    }
    // Unsigned => Real? (no range check needed, as real can hold all unsigned values)
    if (ps_value_get_base(from) == PS_TYPE_UNSIGNED && ps_value_get_base(to) == PS_TYPE_REAL)
    {
        to->data.r = (ps_real)from->data.u;
        return PS_ERROR_NONE;
    }
    return PS_ERROR_TYPE_MISMATCH;
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
    return value

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

char *ps_value_get_enum(const ps_value *value)
{
    if (value == NULL || value->type == NULL || value->type->value == NULL || value->type->value->data.t == NULL ||
        value->type->value->data.t->type != PS_TYPE_ENUM)
        return NULL;
    const ps_type_definition *type_def = value->type->value->data.t;
    ps_symbol **values = type_def->def.e.values;
    ps_enum_value index = value->data.u;
    if (index >= type_def->def.e.count)
        return NULL;
    return values[index]->name;
}

#define NULL_VALUE                                                                                                     \
    {                                                                                                                  \
        if (debug)                                                                                                     \
        {                                                                                                              \
            snprintf(buffer, sizeof(buffer) - 1, "NULL VALUE");                                                        \
            return buffer;                                                                                             \
        }                                                                                                              \
        return NULL;                                                                                                   \
    }

#define NULL_TYPE                                                                                                      \
    {                                                                                                                  \
        if (debug)                                                                                                     \
        {                                                                                                              \
            snprintf(buffer, sizeof(buffer) - 1, "NULL __TYPE__ VALUE");                                               \
            return buffer;                                                                                             \
        }                                                                                                              \
        return NULL;                                                                                                   \
    }

#define ENUM_VALUE                                                                                                     \
    {                                                                                                                  \
        char *enum_value = ps_value_get_enum(value);                                                                   \
        if (debug)                                                                                                     \
            snprintf(buffer, sizeof(buffer) - 1, "%s.%s (%u)", enum_value == NULL ? "NULL" : value->type->name,        \
                     enum_value == NULL ? "NULL" : enum_value, value->data.u);                                         \
        else                                                                                                           \
            snprintf(buffer, sizeof(buffer) - 1, "%s", enum_value == NULL ? "NULL" : enum_value);                      \
        return buffer;                                                                                                 \
    }

#define NONE_TYPE                                                                                                      \
    if (debug)                                                                                                         \
        snprintf(buffer, sizeof(buffer) - 1, "[none]");                                                                \
    else                                                                                                               \
        return NULL;

#define TYPE_DEF_VALUE                                                                                                 \
    if (debug)                                                                                                         \
        snprintf(buffer, sizeof(buffer) - 1, "%s@%p", value->type->name, (void *)(value->data.t));                     \
    else                                                                                                               \
        return NULL;

#define REAL_VALUE                                                                                                     \
    if (width > 0)                                                                                                     \
        if (precision == 0)                                                                                            \
            snprintf(buffer, sizeof(buffer) - 1, "%*" PS_REAL_FMT_WP, width, value->data.r);                           \
        else                                                                                                           \
            snprintf(buffer, sizeof(buffer) - 1, "%*.*" PS_REAL_FMT_WP, width, precision, value->data.r);              \
    else                                                                                                               \
        snprintf(buffer, sizeof(buffer) - 1, "%" PS_REAL_FMT, value->data.r);

#define INTEGER_VALUE                                                                                                  \
    if (debug)                                                                                                         \
        snprintf(buffer, sizeof(buffer) - 1, "%" PS_INTEGER_FMT_10 " / 0x%" PS_UNSIGNED_FMT_16, value->data.i,         \
                 value->data.i);                                                                                       \
    else if (width > 0)                                                                                                \
        snprintf(buffer, sizeof(buffer) - 1, "%*" PS_INTEGER_FMT_10, width, value->data.i);                            \
    else                                                                                                               \
        snprintf(buffer, sizeof(buffer) - 1, "%" PS_INTEGER_FMT_10, value->data.i);

#define UNSIGNED_VALUE                                                                                                 \
    if (debug)                                                                                                         \
        snprintf(buffer, sizeof(buffer) - 1, "%" PS_UNSIGNED_FMT_10 " / 0x%" PS_UNSIGNED_FMT_16, value->data.u,        \
                 value->data.u);                                                                                       \
    else if (width > 0)                                                                                                \
        snprintf(buffer, sizeof(buffer) - 1, "%*" PS_UNSIGNED_FMT_10, width, value->data.i);                           \
    else                                                                                                               \
        snprintf(buffer, sizeof(buffer) - 1, "%" PS_UNSIGNED_FMT_10, value->data.u);

#define CHAR_VALUE                                                                                                     \
    if (debug)                                                                                                         \
        snprintf(buffer, sizeof(buffer) - 1, "'%c' / %03d / 0x%02x", isprint(value->data.c) ? value->data.c : '.',     \
                 value->data.c, value->data.c);                                                                        \
    else                                                                                                               \
        snprintf(buffer, sizeof(buffer) - 1, "%c", value->data.c);

#define STRING_VALUE                                                                                                   \
    if (debug)                                                                                                         \
        if (value->data.s == NULL)                                                                                     \
            snprintf(buffer, sizeof(buffer) - 1, "NULL!");                                                             \
        else                                                                                                           \
            snprintf(buffer, sizeof(buffer) - 1, "%03d/%03d \"%.*s\"", value->data.s->len, value->data.s->max,         \
                     PS_IDENTIFIER_LEN - 10, value->data.s->str);                                                      \
    else                                                                                                               \
    {                                                                                                                  \
        memset(buffer, 0, sizeof(buffer));                                                                             \
        if (value->data.s != NULL)                                                                                     \
            memcpy(buffer, value->data.s->str, value->data.s->len);                                                    \
    }

#define EXECUTABLE_VALUE                                                                                               \
    const ps_executable *executable = value->data.x;                                                                   \
    if (executable == NULL)                                                                                            \
        snprintf(buffer, sizeof(buffer) - 1, "NULL!");                                                                 \
    else if (executable->address != NULL)                                                                              \
        snprintf(buffer, sizeof(buffer) - 1, "SYSTEM@%p", executable->address);                                        \
    else                                                                                                               \
        snprintf(buffer, sizeof(buffer) - 1, "%s@L:%05d/C:%03d",                                                       \
                 executable->formal_signature->result_type == NULL ||                                                  \
                         executable->formal_signature->result_type == &ps_system_none                                  \
                     ? "PROCEDURE"                                                                                     \
                     : "FUNCTION",                                                                                     \
                 executable->line + 1, executable->column + 1);

#define ARRAY_VALUE                                                                                                    \
    if (debug)                                                                                                         \
    {                                                                                                                  \
    }                                                                                                                  \
    else                                                                                                               \
    {                                                                                                                  \
    }

char *ps_value_to_string(const ps_value *value, bool debug, int16_t width, int16_t precision)
{
    static char buffer[PS_STRING_MAX_LEN + 1];
    if (value == NULL)
        NULL_VALUE
    if (value->type->value == NULL)
        NULL_TYPE
    if (value->type->value->data.t->type == PS_TYPE_ENUM)
    {
        const char *enum_value = ps_value_get_enum(value);
        if (debug)
            snprintf(buffer, sizeof(buffer) - 1, "%s.%s (%u)", enum_value == NULL ? "NULL" : value->type->name,
                     enum_value == NULL ? "NULL" : enum_value, value->data.u);
        else
            snprintf(buffer, sizeof(buffer) - 1, "%s", enum_value == NULL ? "NULL" : enum_value);
        return buffer;
    }
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_NONE:
        NONE_TYPE
        break;
    case PS_TYPE_DEFINITION:
        TYPE_DEF_VALUE
        break;
    case PS_TYPE_REAL:
        REAL_VALUE
        break;
    case PS_TYPE_INTEGER:
        INTEGER_VALUE
        break;
    case PS_TYPE_UNSIGNED:
        UNSIGNED_VALUE
        break;
    case PS_TYPE_BOOLEAN:
        snprintf(buffer, sizeof(buffer) - 1, "%s", value->data.b ? "TRUE" : "FALSE");
        break;
    case PS_TYPE_CHAR:
        CHAR_VALUE
        break;
    case PS_TYPE_STRING:
        STRING_VALUE
        break;
    case PS_TYPE_EXECUTABLE:
        EXECUTABLE_VALUE
        break;
    case PS_TYPE_ARRAY:
        // ARRAY_VALUE
        snprintf(buffer, sizeof(buffer) - 1, "ARRAY OF %s", value->type->value->data.t->def.a.item_type->name);
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

char *ps_value_get_display_string(const ps_value *value, int16_t width, int16_t precision)
{
    return ps_value_to_string(value, false, width, precision);
}

char *ps_value_get_debug_string(const ps_value *value)
{
    return ps_value_to_string(value, true, 0, 0);
}

char *ps_value_get_type_name(const ps_value *value)
{
    char *type = "NULL!";
    if (value != NULL)
    {
        if (value->type == NULL)
            type = "TYPE!";
        else
            type = value->type->name;
    }
    return type;
}

char *ps_value_get_base_name(const ps_value *value)
{
    char *base = "NULL!";
    if (value != NULL)
    {
        if (value->type == NULL)
            base = "TYPE!";
        else if (value->type->value == NULL)
            base = "BASE!";
        else
            base = ps_value_type_get_name(value->type->value->data.t->base);
    }
    return base;
}

char *ps_value_dump(const ps_value *value)
{
    static char buffer[512];
    char *type = ps_value_get_type_name(value);
    char *base = ps_value_get_base_name(value);
    char *data = value == NULL ? "NULL!" : ps_value_get_debug_string(value);
    snprintf(buffer, sizeof(buffer) - 1, "VALUE: type=%s (base=%s), value=%s", type, base, data);
    return buffer;
}

void ps_value_debug(FILE *output, const char *message, const ps_value *value)
{
    if (output == NULL)
        output = stderr;
    fprintf(output, "DEBUG\t%s%s\n", message == NULL ? "" : message, ps_value_dump(value));
}

/* EOF */
