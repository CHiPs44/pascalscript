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
        /* simple types => direct value */
        PS_TYPE_NONE = 0,
        PS_TYPE_INTEGER,
        PS_TYPE_UNSIGNED,
        PS_TYPE_REAL,
        PS_TYPE_BOOLEAN,
        PS_TYPE_CHAR,
        PS_TYPE_ENUM,     // future => unsigned value (first=0, second=1, ...), reference to type needed
        PS_TYPE_SUBRANGE, // future => integer value (-10..15), reference to type needed, needed to implement arrays
        PS_TYPE_SET,      // future => unsigned value as a bit field
        PS_TYPE_POINTER,  // future
        /* reference types (pointer to value(s)) */
        PS_TYPE_STRING, // TODO
        PS_TYPE_ARRAY,  // future
        PS_TYPE_RECORD, // future
        PS_TYPE_FILE,   // future
    } __attribute__((__packed__)) ps_type;

    typedef union u_ps_data
    {
        // clang-format off
        ps_integer    i;
        ps_unsigned   u;
        ps_real       r;
        ps_boolean    b;
        ps_char       c;
        ps_pointer    p;
        ps_string_ptr s;
        // clang-format on
    } ps_data;

    typedef struct s_ps_value
    {
        /* clang-format off */
        ps_type type;
        size_t  size;
        ps_data data;
        /* clang-format on */
    } ps_value;

    // clang-format off
    ps_value *ps_value_set_integer (ps_value *value, ps_integer  i);
    ps_value *ps_value_set_unsigned(ps_value *value, ps_unsigned u);
    ps_value *ps_value_set_boolean (ps_value *value, ps_boolean  b);
    ps_value *ps_value_set_char    (ps_value *value, ps_char     c);
    ps_value *ps_value_set_string  (ps_value *value, char *s, ps_string_len max);
    ps_value *ps_value_set_real    (ps_value *value, ps_real     r);
    ps_value *ps_value_set_pointer (ps_value *value, ps_pointer  p);
    // TODO
    ps_integer  ps_value_get_integer (ps_value *value);
    ps_unsigned ps_value_get_unsigned(ps_value *value);
    ps_boolean  ps_value_get_boolean (ps_value *value);
    ps_char     ps_value_get_char    (ps_value *value);
    ps_string  *ps_value_get_string  (ps_value *value);
    ps_real     ps_value_get_real    (ps_value *value);
    ps_pointer  ps_value_get_pointer (ps_value *value);
    // clang-format on

    char *ps_value_get_type_name(ps_type type);
    char *ps_value_get_value(ps_value *value);
    void ps_value_debug(ps_value *value);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VALUE_H */
