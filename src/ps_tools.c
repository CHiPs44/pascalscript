/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>
#include <stdlib.h>
#include <string.h>

ssize_t ps_strscpy(char *dest, const char *src, ssize_t size)
{
    ssize_t i;

    if (size == 0)
        return -EINVAL;

    for (i = 0; i < size - 1 && src[i] != '\0'; i++)
    {
        dest[i] = src[i];
    }
    dest[i] = '\0';

    return (src[i] != '\0') ? -E2BIG : i;
}
