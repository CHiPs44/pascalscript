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

// NB: 32 bits by default, as our far target is RP2040 / ARM M0+

#ifndef PS_INTEGER
// 16 bits
// #define PS_INTEGER int16_t
// #define PS_INTEGER_MIN INT16_MIN
// #define PS_INTEGER_MAX INT32_MAX
// 32 bits
#define PS_INTEGER int32_t
#define PS_INTEGER_MIN INT32_MIN
#define PS_INTEGER_MAX INT32_MAX
// 64 bits
// #define PS_INTEGER int64_t
// #define PS_INTEGER_MIN INT64_MIN
// #define PS_INTEGER_MAX INT64_MAX
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
#define PS_UNSIGNED_MAX UINT32_MAX
// 64 bits
// #define PS_UNSIGNED uint64_t
// #define PS_UNSIGNED_MAX UINT64_MAX
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
#define PS_STRING_MAX UINT8_MAX
#define PS_STRING_NUM_TYPE uint16_t
// #define PS_STRING_LEN_TYPE uint16_t
// #define PS_STRING_MAX UINT16_MAX
#endif

#ifndef PS_STRING_LEN_TYPE
#error PS_STRING_LEN_TYPE must be defined.
#endif

#ifndef PS_STRING_NUM_TYPE
#error PS_STRING_NUM_TYPE must be defined.
#endif

#ifdef __cplusplus
}
#endif

#endif /* _PS_CONFIG_H */
