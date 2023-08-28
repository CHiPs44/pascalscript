/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PASCALSCRIPT_H
#define _PASCALSCRIPT_H

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
#define PS_VERSION_PATCH 1
#endif
#ifndef PS_VERSION_INDEX
#define PS_VERSION_INDEX 0
#endif

#ifndef PS_INTEGER
#define PS_INTEGER int
#endif

#ifndef PS_UNSIGNED_INTEGER
#define PS_UNSIGNED_INTEGER uint
#endif

#ifndef PS_REAL
#define PS_REAL double
#endif

#ifndef PS_CHAR
#define PS_CHAR char
#endif

#ifndef PS_STRING_MAX
#define PS_STRING_MAX 255
#endif

// #ifndef PS_STRING
// #define PS_STRING(__STRING__) PS_CHAR __STRING__[PS_STRING_MAX]
// #endif

#ifdef __cplusplus
}
#endif

#endif /* _PASCALSCRIPT_H */
