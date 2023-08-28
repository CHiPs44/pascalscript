/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _SYMBOL_H
#define _SYMBOL_H

#include <stdlib.h>

#include "pascalscript.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef MAX_SYMBOL_NAME
#define MAX_SYMBOL_NAME 31
#endif

typedef enum _kind_t
{
    KIND_UNKNOWN = 0,
    KIND_AUTO,
    KIND_FREE,
    KIND_CONSTANT,
    KIND_VARIABLE,
    // KIND_PROCEDURE,
    // KIND_FUNCTION,
    // KIND_TYPE,
} kind_t;

typedef enum _type_t
{
    // for constants and variables
    TYPE_NONE = 0,
    TYPE_INTEGER,
    // TYPE_UNSIGNED_INTEGER,
    TYPE_CHAR,
    TYPE_STRING,
    // TYPE_REAL,
    // TYPE_POINTER,
} type_t;

typedef union _value_t
{
    PS_INTEGER          i;
    // PS_UNSIGNED_INTEGER u;
    PS_CHAR             c;
    PS_CHAR             s[PS_STRING_MAX + 1];
    // PS_REAL             r;
} value_t;

typedef struct _symbol_t
{
    char    name[MAX_SYMBOL_NAME + 1];
    kind_t  kind;
    type_t  type;
    size_t  size;
    value_t value;
} symbol_t;

extern void symbol_normalize_name(char *name);

#ifdef __cplusplus
}
#endif

#endif /* _SYMBOL_H */
