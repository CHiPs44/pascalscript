/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>
#include <stdlib.h>
#include <string.h>

size_t ps_strscpy(char *dest, const char *src, size_t size)
{
    size_t i;

    if (size == 0)
    {
        errno = EINVAL;
        return 0;
    }

    for (i = 0; i < size - 1 && src[i] != '\0'; i++)
    {
        dest[i] = src[i];
    }
    dest[i] = '\0';

    if (src[i] == '\0')
        return i;
    else
    {
        errno = E2BIG;
        return 0; // Not enough space to copy the string
    }
}

size_t ps_strnlen(const char *s, size_t maxlen)
{
    size_t len = 0;
    while (len < maxlen && s[len] != '\0')
        ++len;
    return len;
}
