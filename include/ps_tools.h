/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_TOOLS_H
#define _PS_TOOLS_H

#include <string.h>

#ifdef __cplusplus
extern "C"
{
#endif

    /**
     * @brief Our own implementation of strscpy (with int instead of ssize_t)
     * @param dest Destination buffer
     * @param src Source string
     * @param size Size of the destination buffer
     * @return -EINVAL if the size is zero,
     *         -E2BIG if the destination buffer is too small,
     *         the number of characters copied (excluding the null terminator) otherwise
     */
    int ps_strscpy(char *dest, const char *src, int size);

    /** @brief Our own implementation of strnlen (compatible) */
    int ps_strnlen(const char *s, int maxlen);

#ifdef __cplusplus
}
#endif

#endif /* _PS_TOOLS_H */
