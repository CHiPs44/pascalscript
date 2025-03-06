/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
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
    const ps_string_len width = 50;
    static char tmp[128];
    if (s == NULL)
        sprintf(tmp, "NULL");
    else
        snprintf(tmp, sizeof(tmp) - 1,
                 "max=%5d, len=%5d, str=\"%.*s%s\"",
                 s->max, s->len,
                 width, s->str, s->len > width ? "..." : "");
    return tmp;
}

void ps_string_debug(ps_string *s, char *message)
{
    fprintf(stderr, "PS_STRING: %s%s\n", message, ps_string_dump(s));
}

ps_string *ps_string_alloc(ps_string_len max)
{
    // allocate sizeof(max) + sizeof(len) + (max + 1) chars
    ps_string *s = (ps_string *)malloc(
        2 * sizeof(ps_string_len) + (max + 1) * sizeof(ps_char));
    if (s == NULL)
        return NULL; // errno = ENOMEM
    s->max = max;
    s->len = 0;
    return s;
}

void ps_string_free(ps_string *s)
{
    free(s);
}

ps_string *ps_string_set(ps_string *s, ps_char *z)
{
    size_t len = strlen(z);
    if (len > ps_string_max || len > s->max)
    {
        errno = EINVAL;
        return NULL;
    }
    s->len = (ps_string_len)len;
    // TODO
    memcpy(s->str, z);
    return s;
}

ps_string *ps_string_create(ps_string_len max, ps_char *z)
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

ps_string *ps_string_concat(ps_string *a, ps_string *b)
{
    size_t len = a->len + b->len;
    if (len > ps_string_max)
    {
        errno = EINVAL;
        return NULL;
    }
    ps_string *c = ps_string_alloc((ps_string_len)len);
    if (c == NULL)
        return NULL; // errno = ENOMEM
    strncpy(c->str, a->str, c->max);
    strncat(c->str, b->str, c->max);
    c->len = len;
    return c;
}

ps_string *ps_string_get_substring(
    ps_string *a, ps_string_len start, ps_string_len length)
{
    if (start > a->len)
    {
        errno = EINVAL;
        return NULL;
    }
    if (start + length > a->len)
        length = a->len - start;
    ps_string *b = ps_string_alloc(length);
    if (b == NULL)
        return NULL; // errno = ENOMEM
    strncpy(b->str, &a->str[start - 1], length);
    b->len = length;
    return b;
}

int ps_string_compare(ps_string *a, ps_string *b)
{
    return strcmp(a->str, b->str);
}