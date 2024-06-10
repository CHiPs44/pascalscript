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

#ifdef __cplusplus
extern "C"
{
#endif

    typedef PS_INTEGER ps_integer_t;
    const ps_integer_t ps_integer_max = PS_INTEGER_MAX;

    typedef PS_UNSIGNED ps_unsigned_t;
    const ps_unsigned_t ps_unsigned_max = PS_UNSIGNED_MAX;

    typedef PS_BOOLEAN ps_boolean_t;

    typedef PS_CHAR ps_char_t;

    typedef PS_STRING_LEN_TYPE ps_string_len_t;
    const ps_string_len_t ps_string_max = PS_STRING_MAX;
    typedef struct
    {
        ps_string_len_t len;
        ps_char_t str[PS_STRING_MAX]; // TODO replace by a pointer
    } ps_string_t;

    typedef enum _ps_type_t
    {
        // clang-format off
        PS_TYPE_NONE     = 0,
        // Numbers
        PS_TYPE_INTEGER  = 1,
        PS_TYPE_UNSIGNED = 2,
        PS_TYPE_REAL     = 3,
        // Others
        PS_TYPE_BOOLEAN  = 4,
        PS_TYPE_CHAR     = 5,
        PS_TYPE_STRING   = 6,
        PS_TYPE_POINTER  = 7,
        // clang-format on
    } ps_type_t;

    typedef union _ps_data_t
    {
        // clang-format off
        ps_integer_t  i;
        ps_unsigned_t u;
        ps_boolean_t  b;
        ps_char_t     c;
        ps_string_t   s;
        PS_REAL     r;
        void       *p;
        // clang-format on
    } ps_data_t;

    typedef struct _ps_value_t
    {
        ps_type_t type;
        size_t size;
        ps_data_t data;
    } ps_value_t;

    // clang-format off
    ps_value_t *ps_value_integer (ps_value_t *value, ps_integer_t  data);
    ps_value_t *ps_value_unsigned(ps_value_t *value, ps_unsigned_t data);
    ps_value_t *ps_value_boolean (ps_value_t *value, ps_boolean_t  data);
    ps_value_t *ps_value_char    (ps_value_t *value, ps_char_t     data);
    ps_value_t *ps_value_string  (ps_value_t *value, ps_char_t    *data);
    ps_value_t *ps_value_real    (ps_value_t *value, PS_REAL     data);
    ps_value_t *ps_value_pointer (ps_value_t *value, void       *data);
    // clang-format on

    char *value_get_type_name(ps_type_t type);
    char *value_get_value(ps_value_t *value);
    void value_dump(ps_value_t *value);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VALUE_H */
