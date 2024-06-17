/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_TYPES_H
#define _PS_TYPES_H

// #include <stdlib.h>
// #include <stdbool.h>

#include "ps_config.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef PS_INTEGER ps_integer_t;
    const ps_integer_t ps_integer_min = PS_INTEGER_MIN;
    const ps_integer_t ps_integer_max = PS_INTEGER_MAX;

    typedef PS_UNSIGNED ps_unsigned_t;
    const ps_unsigned_t ps_unsigned_max = PS_UNSIGNED_MAX;

    typedef PS_REAL ps_real_t;
    const ps_real_t ps_real_min = PS_REAL_MIN;
    const ps_real_t ps_real_max = PS_REAL_MAX;

    typedef PS_BOOLEAN ps_boolean_t;

    typedef PS_CHAR ps_char_t;

    typedef PS_STRING_LEN_TYPE ps_string_len_t;
    const ps_string_len_t ps_string_max = PS_STRING_MAX;

    typedef struct _ps_string_t
    {
        ps_string_len_t len;
        ps_char_t str[PS_STRING_MAX + 1]; // TODO? replace by a pointer
    } ps_string_t;

    typedef void *ps_pointer_t;

#ifdef __cplusplus
}
#endif

#endif /* _PS_TYPES_H */
