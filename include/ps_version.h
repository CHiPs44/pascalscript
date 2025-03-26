/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_VERSION_H
#define _PS_VERSION_H

#include <stdint.h>
#include <stdlib.h>
// #include <limits.h>
#include <float.h>

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef PS_VERSION_MAJOR
#define PS_VERSION_MAJOR 0
#endif
#ifndef PS_VERSION_MINOR
#define PS_VERSION_MINOR 1
#endif
#ifndef PS_VERSION_PATCH
#define PS_VERSION_PATCH 2
#endif
#ifndef PS_VERSION_INDEX
#define PS_VERSION_INDEX 3
#endif

#ifdef __cplusplus
}
#endif

#endif /* _PS_VERSION_H */
