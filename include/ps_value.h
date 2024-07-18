/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_VALUE_H
#define _PS_VALUE_H

#include <stdlib.h>
#include <stdbool.h>

#include "ps_config.h"
#include "ps_types.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum e_ps_type
    {
        PS_TYPE_NONE = 0,
        PS_TYPE_INTEGER = 1,
        PS_TYPE_UNSIGNED = 2,
        PS_TYPE_REAL = 3,
        PS_TYPE_BOOLEAN = 4,
        PS_TYPE_CHAR = 5,
        PS_TYPE_STRING = 6,
        PS_TYPE_POINTER = 7,
        PS_TYPE_ENUM = 8, // => unsigned value (first=0, second=1, ...)
        PS_TYPE_SUBRANGE = 9, // => integer value (-10..15)
        // future types
        PS_TYPE_SET = 10, // => unsigned value as a bit field
        PS_TYPE_ARRAY = 11,
        PS_TYPE_RECORD = 12,
    } ps_type;

    typedef union u_ps_data
    {
        ps_integer i;
        ps_unsigned u;
        ps_real r;
        ps_boolean b;
        ps_char c;
        ps_string s;
        ps_pointer p;
    } ps_data;

    typedef struct s_ps_value
    {
        ps_type type;
        size_t size;
        ps_data data;
    } ps_value;

    // clang-format off
    ps_value *ps_value_set_integer (ps_value *value, ps_integer  data);
    ps_value *ps_value_set_unsigned(ps_value *value, ps_unsigned data);
    ps_value *ps_value_set_boolean (ps_value *value, ps_boolean  data);
    ps_value *ps_value_set_char    (ps_value *value, ps_char     data);
    ps_value *ps_value_set_string  (ps_value *value, ps_string   data);
    ps_value *ps_value_set_real    (ps_value *value, ps_real     data);
    ps_value *ps_value_set_pointer (ps_value *value, ps_pointer  data);
    // clang-format on

    char *ps_value_get_type_name(ps_type type);
    char *ps_value_get_value(ps_value *value);
    void ps_value_debug(ps_value *value);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VALUE_H */
