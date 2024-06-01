/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_CONFIG_H
#define _PS_CONFIG_H

#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef PS_VERSION_MAJOR
#define PS_VERSION_MAJOR 0
#endif
#ifndef PS_VERSION_MINOR
#define PS_VERSION_MINOR 0
#endif
#ifndef PS_VERSION_PATCH
#define PS_VERSION_PATCH 0
#endif
#ifndef PS_VERSION_INDEX
#define PS_VERSION_INDEX 0
#endif

#ifndef PS_INTEGER
#define PS_INTEGER int32_t
#endif

#ifndef PS_UNSIGNED
#define PS_UNSIGNED uint32_t
#endif

#ifndef PS_BOOLEAN
#define PS_BOOLEAN bool
#endif

#ifndef PS_REAL
// #define PS_REAL double
#define PS_REAL float
#endif

#ifndef PS_CHAR
#define PS_CHAR char
#endif

#ifndef PS_STRING_MAX
#define PS_STRING_MAX 255
#endif

#ifdef __cplusplus
}
#endif

#endif /* _PS_CONFIG_H */
