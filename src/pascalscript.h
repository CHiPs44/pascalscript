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

#ifndef PS_STRING
#define PS_STRING PS_CHAR[PS_STRING_MAX]
#endif

#ifdef __cplusplus
}
#endif

#endif /* _PASCALSCRIPT_H */
