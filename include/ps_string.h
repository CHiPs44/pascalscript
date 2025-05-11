/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_STRING_H
#define _PS_STRING_H

#include <stdio.h>

#include "ps_value.h"

#ifdef __cplusplus
extern 'C'
{
#endif

    /// @brief Dump string to buffer
    char *ps_string_dump(ps_string *s);

    /// @brief Output string to stderr
    void ps_string_debug(FILE *f, char *message, ps_string *s);

    /// @brief Allocate new string of max chars (+1)
    /// @return Newly allocated string or NULL (check errno for ENOMEM)
    ps_string *ps_string_alloc(ps_string_len max);

    /// @brief Free previously allocated string
    void ps_string_free(ps_string *str);

    /// @brief Set existing string to new value if length fits
    /// @return string NULL if len > max
    ps_string *ps_string_set(ps_string *s, char *z);

    /// @brief Allocate and set a new string from a "C" string
    /// @return new string if OK else NULL (check errno for ENOMEM or EINVAL)
    ps_string *ps_string_create(char *z);

    /// @brief Concatenate two strings into another one if lengths are OK
    /// @example concat('ABC', 'DEF') => 'ABCDEF'
    /// @return Newly allocated string or NULL (check errno for ENOMEM or EINVAL)
    ps_string *ps_string_concat(ps_string *a, ps_string *b);

    /// @brief Append string b to string a if lengths are OK
    /// @return a or NULL if error
    ps_string *ps_string_append(ps_string *a, ps_string *b);

    /// @brief Get substring beginning at 'start' for 'length' chars (1 based)
    /// @details substring('ABCDEFGHI',  3, 5) => 'CDEFG'/// @return New string or NULL if error
    /// @example substring('ABCDEFGHI',  7, 1) => 'G'
    /// @example substring('ABCDEFGHI',  1, 9) => 'ABCDEFGHI'
    /// @example substring('ABCDEFGHI', 10, 1) => NULL as 10 > 9
    /// @return Newly allocated string or NULL (check errno for ENOMEM or EINVAL)
    ps_string *ps_string_copy(ps_string *a, ps_string_len start, ps_string_len length);

    /// @brief Concatenate two strings into another one if lengths are OK
    /// @return -1 if a<b, 0 if a=b, 1 if a>b (same as strcmp)
    int ps_string_compare(ps_string *a, ps_string *b);

    /// @brief Search for substring in a string, cf. https://www.freepascal.org/docs-html/rtl/system/pos.html
    /// @example position('ABCDEFGHI', 'CDEFG') => 3
    /// @return position of substring in string or 0 if not found
    ps_string_len ps_string_position(ps_string *substr, ps_string *s);

    /// @brief Get length of string
    /// @example length('ABCDEFGHI') => 9
    /// @param s 
    /// @return 
    ps_string_len ps_string_length(ps_string *s);

    /// @brief Delete chars from a string, cf. https://www.freepascal.org/docs-html/rtl/system/delete.html
    /// @example delete('123456789', 3, 5) => '1289'
    /// @return true if ok, false if error
    ps_string *ps_string_delete(ps_string *s, ps_string_len index, ps_string_len count);

    /// @brief Insert one string into another, cf. https://www.freepascal.org/docs-html/rtl/system/insert.html
    /// @example insert('123456789', 'ABC', 3) => '12ABC3456789'
    /// @return New string or NULL if error
    ps_string *ps_string_insert_string(ps_string *source, ps_string *s, ps_string_len index);

    /// @brief Convert string to lowercase
    /// @example lowercase('AbC') => 'abc'
    /// @return New string or NULL if error
    ps_string *ps_string_lowercase(ps_string *s);

    /// @brief Convert string to uppercase
    /// @example uppercase('aBc') => 'ABC'
    /// @return New string or NULL if error
    ps_string *ps_string_uppercase(ps_string *s);

#ifdef __cplusplus
}
#endif

#endif /* _PS_STRING_H */
