/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_SYSTEM_TYPES_H
#define _PS_SYSTEM_TYPES_H

// #include <stdlib.h>
// #include <stdbool.h>

#include "ps_config.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef PS_INTEGER ps_integer;
    const ps_integer ps_integer_min = PS_INTEGER_MIN;
    const ps_integer ps_integer_max = PS_INTEGER_MAX;

    typedef PS_UNSIGNED ps_unsigned;
    const ps_unsigned ps_unsigned_max = PS_UNSIGNED_MAX;

    typedef PS_REAL ps_real;
    const ps_real ps_real_min = PS_REAL_MIN;
    const ps_real ps_real_max = PS_REAL_MAX;

    typedef PS_BOOLEAN ps_boolean;

    typedef PS_CHAR ps_char;
    const ps_char ps_char_max = PS_CHAR_MAX;

    typedef PS_STRING_LEN_TYPE ps_string_len;
    const ps_string_len ps_string_max = PS_STRING_MAX;

    // typedef PS_STRING_NUM ps_string_num;

    // will be allocated as sizeof(max) + sizeof(len) + (max + 1) ps_chars
    typedef struct s_ps_string
    {
        ps_string_len max;
        ps_string_len len;
        ps_char str[1];
        // ps_string_num num;
    } ps_string;

    typedef void *ps_pointer;

    /*typedef struct s_ps_enum_type
    {
        size_t size;
        char *name;
        char **symbols;
    } ps_enum_type;*/

    /*typedef struct s_ps_file
    {
        bool is_text;
        FILE *handle;
    } ps_file;*/

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYSTEM_TYPES_H */
