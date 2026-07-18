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

    /* @brief Our own implementation of strscpy */
    ssize_t ps_strscpy(char *dest, const char *src, ssize_t size);

#ifdef __cplusplus
}
#endif

#endif /* _PS_TOOLS_H */
