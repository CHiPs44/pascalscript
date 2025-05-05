/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_SYSTEM_TYPES_H
#define _PS_SYSTEM_TYPES_H

#include "ps_config.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef PS_INTEGER ps_integer;
    // typedef PS_INTEGER ps_subrange;
    // const ps_integer ps_integer_min = PS_INTEGER_MIN;
    // const ps_integer ps_integer_max = PS_INTEGER_MAX;

    typedef PS_UNSIGNED ps_unsigned;
    // typedef PS_UNSIGNED ps_enum;
    // const ps_unsigned ps_unsigned_max = PS_UNSIGNED_MAX;

    typedef PS_REAL ps_real;
    // const ps_real ps_real_min = PS_REAL_MIN;
    // const ps_real ps_real_max = PS_REAL_MAX;

    typedef PS_BOOLEAN ps_boolean;
    // const ps_boolean ps_false = false;
    // const ps_boolean ps_true = true;

    typedef PS_CHAR ps_char;
    // const ps_char ps_char_max = PS_CHAR_MAX;

    typedef PS_STRING_LEN_TYPE ps_string_len;
    // const ps_string_len ps_string_max = PS_STRING_MAX_LEN;

    typedef PS_STRING_REF_TYPE ps_string_ref;

    // should be allocated as sizeof(max) + sizeof(len) + (max + 1) ps_chars?
    typedef struct s_ps_string
    {
        ps_string_len max;
        ps_string_len len;
        ps_string_ref ref;
        ps_char str[PS_STRING_MAX_LEN + 1];
    } __attribute__((__packed__)) ps_string;

    // typedef void *ps_pointer;

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

#define PS_INTEGER_SIZE sizeof(ps_integer)
#define PS_UNSIGNED_SIZE sizeof(ps_unsigned)
#define PS_REAL_SIZE sizeof(ps_real)
#define PS_BOOLEAN_SIZE sizeof(ps_boolean)
#define PS_CHAR_SIZE sizeof(ps_char)
// #define PS_STRING_SIZE sizeof(ps_string)

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYSTEM_TYPES_H */
