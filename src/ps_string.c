/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "ps_value.h"
#include "ps_string.h"

char *ps_string_dump(ps_string *s)
{
    const ps_string_len width = PS_IDENTIFIER_LEN;
    static char buffer[128];
    if (s == NULL)
        sprintf(buffer, "NULL");
    else
        snprintf(buffer, sizeof(buffer) - 1,
                 "max=%0*d, len=%0*d, str=\"%.*s%s\"",
                 PS_STRING_MAX_LEN > 255 ? 5 : 3, s->max,
                 PS_STRING_MAX_LEN > 255 ? 5 : 3, s->len,
                 width, s->str, s->len > width ? "..." : "");
    return buffer;
}

void ps_string_debug(FILE *f, char *message, ps_string *s)
{
    if (f == NULL)
        f = stderr;
    if (message == NULL || 0 == strlen(message))
        message = "PS_STRING: ";
    fprintf(f, "DEBUG\t%s%s\n", message, ps_string_dump(s));
}

ps_string *ps_string_alloc(ps_string_len max)
{
    // allocate sizeof(max) + sizeof(len) + (max + 1) chars
    // maximum:          1   +         1   + 255 + 1  bytes for "short" strings
    ps_string *s = (ps_string *)malloc(2 * sizeof(ps_string_len) + (max + 1) * sizeof(ps_char));
    if (s == NULL)
    {
        return NULL;
        errno = ENOMEM;
    }
    s->max = max;
    s->len = 0;
    return s;
}

void ps_string_free(ps_string *s)
{
    free(s);
}

ps_string *ps_string_set(ps_string *s, char *z)
{
    size_t len = strlen(z);
    if (len > s->max)
    {
        errno = EINVAL;
        return NULL;
    }
    s->len = (ps_string_len)len;
    // TODO
    memcpy(s->str, z, len);
    s->str[len] = '\0'; // null terminate
    return s;
}

ps_string *ps_string_create(ps_string_len max, char *z)
{
    ps_string *s = ps_string_alloc(max);
    if (s == NULL)
        return NULL; // errno = ENOMEM
    if (ps_string_set(s, z) == NULL)
    {
        ps_string_free(s);
        return NULL; // errno = EINVAL
    }
    return s;
}

/// @brief Concatenate two strings into another one if lengths are OK
/// @return Newly allocated string or NULL (check errno for ENOMEM or EINVAL)
ps_string *ps_string_concat(ps_string *a, ps_string *b)
{
    size_t len = a->len + b->len;
    if (len > PS_STRING_MAX_LEN)
    {
        errno = EINVAL;
        return NULL;
    }
    ps_string *c = ps_string_alloc((ps_string_len)len);
    if (c == NULL)
    {
        return NULL;
    }
    memcpy(c->str, a->str, a->len);
    memcpy(c->str + a->len, b->str, b->len);
    c->len = len;
    return c;
}

ps_string *ps_string_append(ps_string *a, ps_string *b)
{
    size_t len = a->len + b->len;
    if (len > a->max)
    {
        return NULL; // errno = EINVAL;
    }
    memcpy(a->str + a->len, b->str, b->len);
    a->len = len;
    return a;
}

/// @brief Get substring beginning at "from" for "len" chars (1 based)
/// @details            123456789
///          substring("ABCDEFGHI",  3,  5) => "CDEFG"
///          substring("ABCDEFGHI",  7,  1) => "G"
///          substring("ABCDEFGHI",  1,  9) => "ABCDEFGHI"
///          substring("ABCDEFGHI",  1, 99) => "ABCDEFGHI"
///          substring("ABCDEFGHI", 10,  1) => NULL as 10 > 9
ps_string *ps_string_copy(ps_string *a, ps_string_len from, ps_string_len len)
{
    if (from > a->len)
    {
        return NULL; // errno = EINVAL;
    }
    if (from + len > a->len)
        len = a->len - from;
    ps_string *b = ps_string_alloc(len);
    if (b == NULL)
        return NULL; // errno = ENOMEM
    memcpy(b->str, &a->str[from - 1], len);
    b->len = len;
    return b;
}

/// @brief Compare two strings
/// @return less than 0 if a<b, 0 if a=b, greater then 0 if a>b (same as C strcmp)
int ps_string_compare(ps_string *a, ps_string *b)
{
    // return strcmp(a->str, b->str);
    int diff;
    ps_string_len len = a->len > b->len ? a->len : b->len;
    for (ps_string_len i = 0; i < len; i++)
    {
        if (i < a->len)
        {
            if (i < b->len)
            {
                diff = (int)a->str[i] - (int)b->str[i];
                if (diff == 0)
                {
                    continue;
                }
            }
            else
            {
                diff = (int)a->str[i];
                break;
            }
        }
        else
        {
            diff = -(int)b->str[i];
            break;
        }
    }
    return diff;
}
