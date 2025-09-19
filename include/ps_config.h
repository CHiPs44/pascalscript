/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_CONFIG_H
#define _PS_CONFIG_H

#include <float.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C"
{
#endif

// #define PS_BITNESS 64

/*
    NB: 32 bits by default, as our far target is RP2040 and RP2350 which have either ARM M0+ or M33 cores
        16 bits if someone tries to port PascalScript to an older architecture
        64 bits on actual computers
    cf. <https://en.wiktionary.org/wiki/bitness>
*/
#ifndef PS_BITNESS
#define PS_BITNESS 32
#endif

#ifndef PS_INTEGER

#if PS_BITNESS == 16
#define PS_INTEGER int16_t
#define PS_INTEGER_MIN INT16_MIN
#define PS_INTEGER_MAX INT16_MAX
#define PS_INTEGER_FMT_10 PRId16
#define PS_INTEGER_FMT_16 PRIx16
#endif

#if PS_BITNESS == 32
#define PS_INTEGER int32_t
#define PS_INTEGER_MIN INT32_MIN
#define PS_INTEGER_MAX INT32_MAX
#define PS_INTEGER_FMT_10 PRId32
#define PS_INTEGER_FMT_16 PRIx32
#endif

#if PS_BITNESS == 64
#define PS_INTEGER int64_t
#define PS_INTEGER_MIN INT64_MIN
#define PS_INTEGER_MAX INT64_MAX
#define PS_INTEGER_FMT_10 PRId64
#define PS_INTEGER_FMT_16 PRIx64
#endif

#endif

#if !defined(PS_INTEGER) || !defined(PS_INTEGER_MIN) || !defined(PS_INTEGER_MAX)
#error PS_INTEGER, PS_INTEGER_MIN & PS_INTEGER_MAX must be defined.
#endif

#ifndef PS_UNSIGNED

#if PS_BITNESS == 16
#define PS_UNSIGNED uint16_t
#define PS_UNSIGNED_MAX 0xffff
#define PS_UNSIGNED_FMT_10 PRIu16
#define PS_UNSIGNED_FMT_16 PRIx16
#endif

#if PS_BITNESS == 32
#define PS_UNSIGNED uint32_t
#define PS_UNSIGNED_MAX UINT32_MAX
#define PS_UNSIGNED_FMT_10 PRIu32
#define PS_UNSIGNED_FMT_16 PRIx32
#endif

#if PS_BITNESS == 64
#define PS_UNSIGNED uint64_t
#define PS_UNSIGNED_MAX UINT64_MAX
#define PS_UNSIGNED_FMT_10 PRIu64
#define PS_UNSIGNED_FMT_16 PRIx64
#endif

#endif

#if !defined(PS_UNSIGNED) || !defined(PS_UNSIGNED_MAX)
#error PS_UNSIGNED & PS_UNSIGNED_MAX must be defined.
#endif

#ifndef PS_BOOLEAN
#define PS_BOOLEAN bool
#endif

#ifndef PS_REAL

#if PS_BITNESS == 16
// float16 support? Is it enough? Is it better than nothing?
//  => default to 4 bytes floats for now
#define PS_REAL float
#define PS_REAL_MIN FLT_MIN
#define PS_REAL_MAX FLT_MAX
#define PS_REAL_EPSILON FLT_EPSILON
#define PS_REAL_FMT "8G"
#endif

#if PS_BITNESS == 32
#define PS_REAL float
#define PS_REAL_MIN FLT_MIN
#define PS_REAL_MAX FLT_MAX
#define PS_REAL_EPSILON FLT_EPSILON
#define PS_REAL_FMT "8G"
#endif

#if PS_BITNESS == 64
#define PS_REAL double
#define PS_REAL_MIN DBL_MIN
#define PS_REAL_MAX DBL_MAX
#define PS_REAL_EPSILON DBL_EPSILON
#define PS_REAL_FMT "15lG"
#endif

#endif

#ifndef PS_REAL_MIN
#error PS_REAL_MIN must be defined.
#endif

#ifndef PS_REAL_MAX
#error PS_REAL_MAX must be defined.
#endif

#ifndef PS_REAL_EPSILON
#error PS_REAL_EPSILON must be defined.
#endif

#ifndef PS_CHAR
// #define PS_CHAR unsigned char (does not work for now)
// #define PS_CHAR char
#define PS_CHAR uint8_t
#define PS_CHAR_MAX UINT8_MAX
#endif

#ifndef PS_STRING_MAX_LEN
// "Short" strings
#define PS_STRING_LEN_TYPE uint8_t
#define PS_STRING_MAX_LEN 255u
// "Wide" strings
// #define PS_STRING_LEN_TYPE uint16_t
// #define PS_STRING_MAX_LEN UINT16_MAX
// "Ultra-wide" strings
// #define PS_STRING_LEN_TYPE uint32_t
// #define PS_STRING_MAX_LEN UINT32_MAX
#endif

#ifndef PS_STRING_REF_TYPE
#define PS_STRING_REF_TYPE uint16_t
#endif

#if !defined(PS_STRING_LEN_TYPE) || !defined(PS_STRING_MAX_LEN) || !defined(PS_STRING_REF_TYPE)
#error PS_STRING_LEN_TYPE / PS_STRING_MAX_LEN / PS_STRING_REF_TYPE must be defined.
#endif

#ifndef PS_IDENTIFIER_LEN
#define PS_IDENTIFIER_LEN 31u
#endif

#define PS_IDENTIFIER_SIZE (PS_IDENTIFIER_LEN + 1)

    typedef char ps_identifier[PS_IDENTIFIER_SIZE];

#ifdef __cplusplus
}
#endif

#endif /* _PS_CONFIG_H */
