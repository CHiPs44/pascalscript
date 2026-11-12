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
extern "C"
{
#endif

    /// @brief Allocate new string of max chars (+1 for NUL terminator)
    /// @return Newly allocated string or NULL (check errno for ENOMEM)
    ps_string *ps_string_alloc(ps_string_len max);

    /// @brief Free previously allocated string
    /// @return NULL
    ps_string *ps_string_free(ps_string *str);

    /// @brief Set existing string to new value if length fits (no grow)
    /// @return input string or NULL if len > max
    ps_string *ps_string_set(ps_string *s, ps_string_len len, const char *z);

    /// @brief Allocate and set a new string from a null terminated "C" string
    /// @example create("Hello") => "Hello"
    /// @example create("...String longer than 255 chars...") =>  NULL with errno = EINVAL
    /// @return new string if OK else NULL (check errno for ENOMEM or EINVAL)
    ps_string *ps_string_create(const char *z);

    /// @brief Grow string up to len chars upper rounded to next multiple of 16
    /// @example ps_string_grow(s, 23) will round max length to (23 + 16) & ~&15 => 39 & ~15 => 32
    /// @return reallocated string if OK else NULL (check errno for ENOMEM or EINVAL)
    ps_string *ps_string_grow(ps_string *s, ps_string_len len);

    /// @brief Allocate and set a new string from a char
    /// @example ps_string_create_char('A') => "A"
    /// @return new string if OK else NULL (check errno for ENOMEM)
    ps_string *ps_string_create_char(ps_char c);

    /// @brief Concatenate two strings into another one if lengths are OK
    /// @example ps_string_concat("ABC", "DEF") => "ABCDEF"
    /// @return Newly allocated string or NULL (check errno for ENOMEM or EINVAL)
    ps_string *ps_string_concat(const ps_string *a, const ps_string *b);

    /// @brief Concatenate two chars into a new string
    /// @example ps_string_concat('A', 'B') => "AB"
    /// @return Newly allocated string or NULL (check errno for ENOMEM)
    ps_string *ps_string_concat_chars(ps_char a, ps_char b);

    /// @brief Append string b to string a, growing it if necessary
    /// @example ps_string_append("ABC", "DEF") => "ABCDEF"
    /// @return a (or reallocated a) or NULL if error
    ps_string *ps_string_append(ps_string *a, const ps_string *b);

    /// @brief Append char to string, growing it if necessary
    /// @example ps_string_append_char("ABC", 'D') => "ABCD"
    /// @return string (or reallocated string) or NULL (check errno ENOMEM or EINVAL)
    ps_string *ps_string_append_char(ps_string *s, ps_char c);

    /// @brief Get substring beginning at 'start' for 'length' chars (1 based)
    /// @example substring('ABCDEFGHI',  3, 5) => "CDEFG"
    /// @example substring('ABCDEFGHI',  7, 1) => "G"
    /// @example substring('ABCDEFGHI',  1, 9) => "ABCDEFGHI"
    /// @example substring('ABCDEFGHI', 10, 1) => NULL as 10 > 9
    /// @return Newly allocated string or NULL (check errno for ENOMEM or EINVAL)
    ps_string *ps_string_copy(const ps_string *a, ps_string_len from, ps_string_len len);

    /// @brief Compare two strings, using C "strcmp" for now
    /// @return -1 if a<b, 0 if a=b, 1 if a>b
    int ps_string_compare(ps_string *a, ps_string *b);

    /// @brief Search for substring in a string, cf. https://www.freepascal.org/docs-html/rtl/system/pos.html
    /// @example ps_string_position("CDEFG", "ABCDEFGHI") => 3
    /// @return Position of substring in string or 0 if not found
    ps_string_len ps_string_position(ps_string *substr, ps_string *s);

    /// @brief Get length of string, cf. https://www.freepascal.org/docs-html/rtl/system/length.html
    /// @example ps_string_length('ABCDEFGHI') => 9
    /// @return Current length of string
    ps_string_len ps_string_length(ps_string *s);

    /// @brief Delete chars from a string, cf. https://www.freepascal.org/docs-html/rtl/system/delete.html
    /// @example delete('123456789', 3, 5) => '1289'
    /// @return true if ok, false if error
    ps_string *ps_string_delete(ps_string *s, ps_string_len index, ps_string_len count);

    /// @brief Insert one string into another, cf. https://www.freepascal.org/docs-html/rtl/system/insert.html
    /// @example insert('123456789', 'ABC', 3) => '12ABC3456789'
    /// @return New string or NULL if error
    ps_string *ps_string_insert_string(ps_string *source, const ps_string *s, ps_string_len index);

    /// @brief Convert string to lowercase as a new string
    /// @example lowercase('AbC') => 'abc'
    /// @return New string or NULL if error
    ps_string *ps_string_lowercase(const ps_string *s);

    /// @brief Convert string to uppercase as a new string
    /// @example uppercase('aBc') => 'ABC'
    /// @return New string or NULL if error
    ps_string *ps_string_uppercase(const ps_string *s);

    /// @brief Dump first PS_IDENTIFIER_MAX chars of string, with max and length prefix
    /// @example ps_string_dump('012345679ABCDEFGHIJKLMNOPQRSTUVWXYZ012345679') => 009/255 str="012345679ABCDEFGHIJKLMNOPQRSTU"
    /// @return static buffer
    char *ps_string_dump(ps_string *s);

    /// @brief Output string to stderr
    void ps_string_debug(FILE *f, char *message, ps_string *s);

#ifdef __cplusplus
}
#endif

#endif /* _PS_STRING_H */
