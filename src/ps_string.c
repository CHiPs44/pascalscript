/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_string.h"
#include "ps_value.h"

ps_string *ps_string_alloc(ps_string_len max)
{
    // allocate sizeof(max) + sizeof(len) + (max + 1) chars
    // maximum: 1           + 1           +  255 + 1  bytes for "short" strings
    //       => 258
    ps_string *s = (ps_string *)malloc(2 * sizeof(ps_string_len) + (max + 1) * sizeof(ps_char));
    if (s == NULL)
        return NULL; // errno = ENOMEM;
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

ps_string *ps_string_create(char *z, ps_string_len max)
{
    size_t len = strlen(z);
    if (max == 0)
        max = PS_STRING_MAX_LEN; // default max length
    if (len > max)
    {
        errno = EINVAL;
        return NULL;
    }
    ps_string *s = ps_string_alloc((ps_string_len)len);
    if (s == NULL)
        return NULL; // errno = ENOMEM
    if (ps_string_set(s, z) == NULL)
    {
        ps_string_free(s);
        return NULL; // errno = EINVAL
    }
    return s;
}

ps_string *ps_string_create_char(ps_char c)
{
    ps_string *s = ps_string_alloc(1);
    if (s == NULL)
        return NULL; // errno = ENOMEM
    s->str[0] = c;
    s->str[1] = '\0'; // null terminator
    s->len = 1;
    return s;
}

ps_string *ps_string_concat(ps_string *a, ps_string *b, ps_string_len max)
{
    size_t len = a->len + b->len;
    // exit if max length is specified and exceeded
    if (max > 0)
    {
        if (len > max)
        {
            errno = EINVAL;
            return NULL;
        }
    }
    else
    {
        max = PS_STRING_MAX_LEN;
        // truncate if max length is exceeded
        if (len > max)
        {
            len = PS_STRING_MAX_LEN;
        }
    }
    ps_string *c = ps_string_alloc((ps_string_len)len);
    if (c == NULL)
    {
        return NULL; // errno = ENOMEM
    }
    memcpy(c->str, a->str, a->len);
    memcpy(c->str + a->len, b->str, b->len);
    c->str[len] = '\0'; // null terminate
    c->len = len;
    return c;
}

ps_string *ps_string_concat_chars(ps_char a, ps_char b)
{
    ps_string *s = ps_string_alloc(2);
    if (s == NULL)
        return NULL; // errno = ENOMEM
    s->str[0] = a;
    s->str[1] = b;
    s->str[2] = '\0'; // null terminate
    s->len = 2;
    return s;
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

ps_string *ps_string_copy(ps_string *a, ps_string_len from, ps_string_len len)
{
    if (from > a->len)
    {
        errno = EINVAL;
        return NULL;
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
/// @return less than 0 if a<b, 0 if a=b, greater than 0 if a>b (uses C strcmp)
int ps_string_compare(ps_string *a, ps_string *b)
{
    return strcmp((char *)a->str, (char *)b->str);
    // int diff = 0;
    // ps_string_len len = a->len > b->len ? a->len : b->len;
    // for (ps_string_len i = 0; i < len; i++)
    // {
    //     if (i < a->len)
    //     {
    //         if (i < b->len)
    //         {
    //             diff = (int)a->str[i] - (int)b->str[i];
    //             if (diff == 0)
    //             {
    //                 continue;
    //             }
    //         }
    //         else
    //         {
    //             diff = (int)a->str[i];
    //             break;
    //         }
    //     }
    //     else
    //     {
    //         diff = -(int)b->str[i];
    //         break;
    //     }
    // }
    // return diff;
}

ps_string_len ps_string_position(ps_string *substr, ps_string *s)
{
    return 0; // TODO
}

ps_string *ps_string_delete(ps_string *s, ps_string_len index, ps_string_len count)
{
    return NULL; // TODO
}

ps_string *ps_string_insert_string(ps_string *source, ps_string *s, ps_string_len index)
{
    return NULL; // TODO
}

ps_string *ps_string_lowercase(ps_string *s)
{
    ps_string *t = ps_string_alloc(s->max);
    if (t == NULL)
        return NULL; // errno = ENOMEM
    for (ps_string_len i = 0; i < s->len; i++)
    {
        if (s->str[i] >= 'A' && s->str[i] <= 'Z')
            t->str[i] = s->str[i] + ('a' - 'A'); // PLUS!
        else
            t->str[i] = s->str[i];
    }
    t->len = s->len;
    return t;
}

ps_string *ps_string_uppercase(ps_string *s)
{
    ps_string *t = ps_string_alloc(s->max);
    if (t == NULL)
        return NULL; // errno = ENOMEM
    for (ps_string_len i = 0; i < s->len; i++)
    {
        if (s->str[i] >= 'a' && s->str[i] <= 'z')
            t->str[i] = s->str[i] - ('a' - 'A'); // MINUS!
        else
            t->str[i] = s->str[i];
    }
    t->len = s->len;
    return t;
}

char *ps_string_dump(ps_string *s)
{
    const ps_string_len width = PS_IDENTIFIER_LEN;
    static char buffer[128];
    if (s == NULL)
        sprintf(buffer, "NULL");
    else
        snprintf(buffer, sizeof(buffer) - 1, "max=%0*d, len=%0*d, str=\"%.*s%s\"", PS_STRING_MAX_LEN > 255 ? 5 : 3,
                 s->max, PS_STRING_MAX_LEN > 255 ? 5 : 3, s->len, width, s->str, s->len > width ? "..." : "");
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
