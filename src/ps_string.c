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

#include "ps_char.h"
#include "ps_memory.h"
#include "ps_string.h"
#include "ps_value.h"

ps_string *ps_string_alloc(ps_string_len max)
{
    if (max == 0)
        return NULL;
    size_t size = sizeof(ps_string) + (max + 1) * sizeof(ps_char);
    ps_string *s = (ps_string *)ps_memory_malloc(PS_MEMORY_STRING, size);
    if (s == NULL)
        return NULL; // errno = ENOMEM
    s->max = max;
    s->len = 0;
    // for now, ensure allocated memory is cleared
    memset(s->str, '\0', max + 1);
    return s;
}

ps_string *ps_string_free(ps_string *s)
{
    if (s != NULL)
        ps_memory_free(PS_MEMORY_STRING, s);
    return NULL;
}

ps_string *ps_string_set(ps_string *s, ps_string_len len, const char *z)
{
    if (len > s->max)
    {
        errno = EINVAL; // or E2BUG?
        return NULL;
    }
    s->len = (ps_string_len)len;
    if (z != NULL)
        memcpy(s->str, z, len);
    s->str[len] = '\0'; // null terminate
    return s;
}

ps_string *ps_string_create(const char *z)
{
    size_t len = z == NULL ? 0 : strlen(z); // NOSONAR
    if (len > PS_STRING_MAX_LEN)
        return NULL;
    ps_string *s = ps_string_alloc((ps_string_len)len);
    if (s == NULL)
        return NULL; // errno = ENOMEM
    if (ps_string_set(s, z, (ps_string_len)len, z) == NULL)
    {
        ps_string_free(s);
        errno = EINVAL;
        return NULL;
    }
    return s;
}

ps_string *ps_string_grow(ps_string *s, ps_string_len max)
{
    if (max <= s->max)
        return NULL;
    size_t size = sizeof(ps_string) + (max + 1) * sizeof(ps_char);
    s = (ps_string *)ps_memory_realloc(PS_MEMORY_STRING, s, size);
    if (s == NULL)
        return NULL; // errno = ENOMEM
    s->max = max;
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

ps_string *ps_string_concat(const ps_string *a, const ps_string *b)
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
        return NULL; // errno = ENOMEM
    }
    memcpy(c->str, a->str, a->len);
    memcpy(c->str + a->len, b->str, b->len);
    c->str[len] = '\0'; // null terminate
    c->len = (ps_string_len)len;
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

ps_string *ps_string_append(ps_string *a, const ps_string *b)
{
    size_t len = a->len + b->len;
    if (len > PS_STRING_MAX_LEN)
    {
        errno = EINVAL;
        return NULL;
    }
    ps_string *c = ps_string_grow(a, (ps_string_len)len);
    if (c == NULL)
        return NULL; // errno = ENOMEM
    memcpy(a->str + a->len, b->str, b->len);
    a->len = (ps_string_len)len;
    return a;
}

ps_string *ps_string_copy(const ps_string *a, ps_string_len from, ps_string_len len)
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
    b->str[len] = '\0';
    return b;
}

/// @brief Compare two strings
/// @return less than 0 if a<b, 0 if a=b, greater than 0 if a>b (uses C strcmp)
int ps_string_compare(ps_string *a, ps_string *b)
{
    // just use strcmp for now
    return strcmp((char *)a->str, (char *)b->str);
}

ps_string_len ps_string_position(ps_string *needle, ps_string *haystack)
{
    const char *pos = strstr((char *)haystack->str, (char *)needle->str);
    if (pos == NULL)
        return 0;

    return (ps_string_len)(pos - (char *)haystack->str + 1);
}

ps_string *ps_string_delete(ps_string *s, ps_string_len index, ps_string_len count)
{
    if (index < 1 || index > s->len)
    {
        errno = EINVAL;
        return NULL;
    }
    if (count == 0)
        return s; // nothing to delete
    if (index + count - 1 > s->len)
    {
        count = s->len - index + 1; // adjust count
    }
    memmove(&s->str[index - 1], &s->str[index - 1 + count], s->len - (index - 1 + count));
    s->len -= count;

    return s;
}

ps_string *ps_string_insert_string(ps_string *source, const ps_string *s, ps_string_len index)
{
    if (source == NULL || s == NULL)
    {
        errno = EINVAL;
        return NULL;
    }
    if (index < 1 || index > source->len + 1)
    {
        errno = EINVAL;
        return NULL;
    }
    // Nothing to insert?
    if (s->len == 0)
        return source;
    // Check if there's enough space
    if (source->len + s->len > source->max)
    {
        errno = EINVAL;
        return NULL;
    }
    // Shift characters to the right to make space
    size_t pos = index - 1;
    memmove(&source->str[pos + s->len], &source->str[pos], source->len - pos);
    // Copy the new string
    memcpy(&source->str[pos], s->str, s->len);
    // Update length
    source->len += s->len;
    // Ensure null termination
    source->str[source->len] = '\0';

    return source;
}

ps_string *ps_string_lowercase(const ps_string *s)
{
    ps_string *t = ps_string_alloc(s->max);
    if (t == NULL)
        return NULL; // errno = ENOMEM

    for (ps_string_len i = 0; i < s->len; i++)
        t->str[i] = ps_char_lowercase(s->str[i]);
    t->len = s->len;
    return t;
}

ps_string *ps_string_uppercase(const ps_string *s)
{
    ps_string *t = ps_string_alloc(s->max);
    if (t == NULL)
        return NULL; // errno = ENOMEM
    for (ps_string_len i = 0; i < s->len; i++)
        t->str[i] = ps_char_uppercase(s->str[i]);
    t->len = s->len;
    return t;
}

char *ps_string_dump(ps_string *s)
{
    const ps_string_len width = PS_IDENTIFIER_LEN;
    static char buffer[128];
    if (s == NULL)
        snprintf(buffer, sizeof(buffer), "NULL");
    else
        snprintf(buffer, sizeof(buffer), "max=%0*d, len=%0*d, str=\"%.*s%s\"", PS_STRING_MAX_LEN > 255 ? 5 : 3, s->max,
                 PS_STRING_MAX_LEN > 255 ? 5 : 3, s->len, width, s->str, s->len > width ? "..." : "");
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
