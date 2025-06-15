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

    /** @brief signed integer */
    typedef PS_INTEGER ps_integer;

    /** @brief unsigned integer */
    typedef PS_UNSIGNED ps_unsigned;

    /** @brief float/double */
    typedef PS_REAL ps_real;

    /** @brief false or true (not an enum) */
    typedef PS_BOOLEAN ps_boolean;

    /** @brief character */
    typedef PS_CHAR ps_char;

    /** @brief string length */
    typedef PS_STRING_LEN_TYPE ps_string_len;

    /** @brief string: max chars + actuel length + string itself, zero terminated */
    // NB: should be allocated as sizeof(max) + sizeof(len) + (max + 1) ps_chars
    typedef struct s_ps_string
    {
        ps_string_len max;
        ps_string_len len;
        ps_char str[1];
    } __attribute__((__packed__)) ps_string;

    /** @brief  */
    typedef void *ps_pointer;

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
#define PS_STRING_SIZE sizeof(ps_string)

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYSTEM_TYPES_H */
