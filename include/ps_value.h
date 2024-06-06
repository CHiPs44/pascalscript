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

    typedef enum _ps_type_t
    {
        // clang-format off
        PS_TYPE_NONE     = 0,
        PS_TYPE_INTEGER  = 1,
        PS_TYPE_UNSIGNED = 2,
        PS_TYPE_CHAR     = 3,
        PS_TYPE_STRING   = 4,
        PS_TYPE_BOOLEAN  = 5,
        PS_TYPE_REAL     = 6,
        PS_TYPE_POINTER  = 7,
        // clang-format on
    } ps_type_t;

    typedef union _ps_ps_data_t
    {
        // clang-format off
        PS_INTEGER  i;
        PS_UNSIGNED u;
        PS_BOOLEAN  b;
        PS_CHAR     c;
        PS_CHAR     s[PS_STRING_MAX + 1]; // TODO replace by a pointer
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
    ps_value_t *ps_value_integer (ps_value_t *value, PS_INTEGER  data);
    ps_value_t *ps_value_unsigned(ps_value_t *value, PS_UNSIGNED data);
    ps_value_t *ps_value_boolean (ps_value_t *value, PS_BOOLEAN  data);
    ps_value_t *ps_value_char    (ps_value_t *value, PS_CHAR     data);
    ps_value_t *ps_value_string  (ps_value_t *value, PS_CHAR    *data);
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
