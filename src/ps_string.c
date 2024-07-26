/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_value.h"
#include "ps_string.h"

ps_string *ps_string_new(ps_string_len max)
{
    ps_string *ptr;
    // allocate sizeof(max) + sizeof(len) + (max + 1) chars
    ptr = (ps_string *)malloc(2 * sizeof(ps_string_len) + (max + 1) * sizeof(ps_char));
    if (ptr == NULL)
        return NULL; // errno = ENOMEM
    ptr->max = max;
    ptr->len = 0;
    return ptr;
}

void ps_string_free(ps_string *str)
{
    free(str);
}

bool ps_string_set(ps_string *ptr, ps_char *zstr)
{
    size_t len = strlen(zstr);
    if (len > ps_string_max)
        return false;
    if (len > ptr->max)
        return false;
    ptr->len = (ps_string_len)len;
    strcpy(ptr->str, zstr);
    return true;
}

ps_string *ps_string_concat(ps_string *a, ps_string *b)
{
    size_t len = a->len + b->len;
    if (len > ps_string_max)
        return NULL;
    ps_string *c = ps_string_new((ps_string_len)len);
    if (c == NULL)
        return NULL; // errno = ENOMEM
    strncpy(c->str, a->str, c->max);
    strncat(c->str, b->str, c->max);
    return c;
}

ps_string *ps_string_get_slice(ps_string *a, ps_string_len from, ps_string_len to)
{
    if (from > to)
        return NULL;
    ps_string *c = ps_string_new(to - from + 1);
    if (c == NULL)
        return NULL; // errno = ENOMEM
    strncpy(c->str, &a->str[from], to - from + 1);
    return c;
}
