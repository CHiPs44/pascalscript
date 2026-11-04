/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>

int ps_strscpy(char *dest, const char *src, int size)
{
    int i;

    if (size <= 0)
        return -EINVAL;

    for (i = 0; i < size - 1 && src[i] != '\0'; i++)
    {
        dest[i] = src[i];
    }
    dest[i] = '\0';

    if (src[i] == '\0')
        return i;
    else
        return -E2BIG; // Not enough space to copy the string
}

int ps_strnlen(const char *s, int maxlen)
{
    int len = 0;
    while (len < maxlen && s[len] != '\0')
        ++len;
    return len;
}
