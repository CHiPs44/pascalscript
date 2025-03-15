/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_STRING_H
#define _PS_STRING_H

#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /// @brief Dump string to buffer
    char *ps_string_dump(ps_string *s);

    /// @brief Output string to stderr
    void ps_string_debug(ps_string *s, char *message);

    /// @brief Allocate new string of max chars (+1)
    /// @return Newly allocated string or NULL (check errno for ENOMEM)
    ps_string *ps_string_new(ps_string_len max);

    /// @brief Free previously allocated string
    void ps_string_free(ps_string *str);

    /// @brief Set existing string to new value if length fits
    /// @return string NULL if len > max
    ps_string *ps_string_set(ps_string *s, ps_char *z);

    // TODO? bool ps_string_copy(ps_string *s1, ps_string *s2);

    /// @brief Allocate and set a new string
    /// @return new string if OK else NULL (check errno for ENOMEM or EINVAL)
    ps_string *ps_string_create(ps_string_len max, ps_char *z);

    /// @brief Concatenate two strings into another one if lengths are OK
    /// @details concat("ABC", "DEF") => "ABCDEF"
    /// @return Newly allocated string or NULL (check errno for ENOMEM or EINVAL)
    ps_string *ps_string_concat(ps_string *a, ps_string *b);

    /// @brief Get substring beginning at "start" for "length" chars (1 based)
    /// @details substring("ABCDEFGHI", 3, 5) => "CDEFG"
    ///          substring("ABCDEFGHI", 7, 1) => "G"
    ///          substring("ABCDEFGHI", 1, 9) => "ABCDEFGHI"
    /// @return Newly allocated string or NULL (check errno for ENOMEM or EINVAL)
    ps_string *ps_string_substring(ps_string *a, ps_string_len start, ps_string_len length);

    /// @brief Concatenate two strings into another one if lengths are OK
    /// @return -1 if a<b, 0 if a=b, 1 if a>b (same as strcmp)
    int ps_string_compare(ps_string *a, ps_string *b);

#ifdef __cplusplus
}
#endif

#endif /* _PS_STRING_H */
