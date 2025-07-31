/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_SYSTEM_TYPES_H
#define _PS_SYSTEM_TYPES_H

#include "ps_config.h"
#include "ps_type_definition.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct s_ps_type_definition ps_type_definition;

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

    /** @brief string length type */
    typedef PS_STRING_LEN_TYPE ps_string_len;

    /** @brief string: max chars + actual length + string itself, zero terminated */
    /** @note should be allocated as sizeof(max) + sizeof(len) + (max + 1) ps_chars */
    typedef struct s_ps_string
    {
        ps_string_len max;                  /** @brief max length */
        ps_string_len len;                  /** @brief actual length */
        ps_char str[PS_STRING_MAX_LEN + 1]; /** @brief string itself, zero terminated for C compatibility */
    } __attribute__((__packed__)) ps_string;

    // typedef ps_unsigned ps_set_handle; /** @brief Set type handle */
    typedef uint8_t ps_set[8]; /** @brief Set */

    typedef struct s_ps_file
    {
        ps_type_definition *type; /** @brief Type definition of the file */
        FILE *f;                  /** @brief "C" file pointer */
        bool is_text;             /** @brief true if text file, false if binary */
    } __attribute__((__packed__)) ps_file;

    /** @brief  */
    typedef void *ps_pointer;

#define PS_INTEGER_SIZE sizeof(ps_integer)
#define PS_UNSIGNED_SIZE sizeof(ps_unsigned)
#define PS_REAL_SIZE sizeof(ps_real)
#define PS_BOOLEAN_SIZE sizeof(ps_boolean)
#define PS_CHAR_SIZE sizeof(ps_char)
#define PS_STRING_SIZE sizeof(ps_string)
#define PS_FILE_SIZE sizeof(ps_file)

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYSTEM_TYPES_H */
