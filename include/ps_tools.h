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
     * @brief Our own implementation of strscpy (incompatible)
     *
     *        Changes the value of errno to EINVAL if size is 0,
     *        or E2BIG if the string is too long
     * @param dest Destination buffer
     * @param src Source string
     * @param size Size of the destination buffer
     * @return The number of characters copied (excluding the null terminator) or 0
     */
    size_t ps_strscpy(char *dest, const char *src, size_t size);

    /** @brief Our own implementation of strnlen (compatible) */
    size_t ps_strnlen(const char *s, size_t maxlen);

#ifdef __cplusplus
}
#endif

#endif /* _PS_TOOLS_H */
