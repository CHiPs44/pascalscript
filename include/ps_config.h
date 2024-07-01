/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_CONFIG_H
#define _PS_CONFIG_H

#include <stdint.h>
#include <stdlib.h>
// #include <limits.h>
#include <float.h>

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef PS_INTEGER
// 16 bits
// #define PS_INTEGER int16_t
// #define PS_INTEGER_MIN -32768
// #define PS_INTEGER_MAX 32767
// 32 bits
#define PS_INTEGER int32_t
#define PS_INTEGER_MIN -2147483648L
#define PS_INTEGER_MAX 2147483647L
#endif

#ifndef PS_INTEGER_MIN
#error PS_INTEGER_MIN must be defined.
#endif

#ifndef PS_INTEGER_MAX
#error PS_INTEGER_MAX must be defined.
#endif

#ifndef PS_UNSIGNED
// 16 bits
// #define PS_UNSIGNED uint16_t
// #define PS_UNSIGNED_MAX 0xffff
// 32 bits
#define PS_UNSIGNED uint32_t
#define PS_UNSIGNED_MAX 0xffffffff
#endif

#ifndef PS_UNSIGNED_MAX
#error PS_UNSIGNED_MAX must be defined.
#endif

#ifndef PS_BOOLEAN
#define PS_BOOLEAN bool
#endif

#ifndef PS_REAL
#define PS_REAL float
#define PS_REAL_MIN FLT_MIN
#define PS_REAL_MAX FLT_MAX
// #define PS_REAL double
// #define PS_REAL_MIN DBL_MIN
// #define PS_REAL_MAX DBL_MAX
#endif

#ifndef PS_CHAR
#define PS_CHAR char
#endif

#ifndef PS_STRING_MAX
#define PS_STRING_LEN_TYPE uint8_t
#define PS_STRING_MAX 255
// #define PS_STRING_LEN_TYPE uint16_t
// #define PS_STRING_MAX 65535
#endif

#ifndef PS_STRING_LEN_TYPE
#error PS_STRING_LEN_TYPE must be defined.
#endif

#ifdef __cplusplus
}
#endif

#endif /* _PS_CONFIG_H */
