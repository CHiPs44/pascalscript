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

ps_string *ps_string_new(ps_string_len max, ps_char *str)
{
    ps_string *ptr;
    size_t len = strlen(str);
    if (len > ps_string_max)
        return NULL;
    if (len > max)
        return NULL;
    // sizeof(max) + sizeof(len) + max chars
    ptr = (ps_string *)malloc(2 * sizeof(ps_string_len) + max * sizeof(ps_char));
    if (ptr == NULL)
        return NULL;
    ptr->max = max;
    ptr->len = len;
    strcpy(ptr->str, str);
    return ptr;
}

    void ps_string_free(ps_string *str)
    {
        free(str);
    }
