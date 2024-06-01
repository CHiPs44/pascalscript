/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_SYMBOL_H
#define _PS_SYMBOL_H

#include <stdlib.h>
#include <stdbool.h>

#include "ps_config.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef MAX_SYMBOL_NAME
#define MAX_SYMBOL_NAME 31
#endif

typedef enum _kind_t
{
    KIND_FREE = 0,
    KIND_AUTO,
    KIND_CONSTANT,
    KIND_VARIABLE,
    KIND_PROCEDURE,
    KIND_FUNCTION,
    KIND_TYPE,
} kind_t;

typedef enum _type_t
{
    // for constants and variables
    TYPE_NONE = 0,
    TYPE_INTEGER,
    TYPE_UNSIGNED_INTEGER,
    TYPE_CHAR,
    TYPE_STRING,
    TYPE_BOOLEAN,
    TYPE_REAL,
    TYPE_POINTER,
} type_t;

#define PS_SCOPE_GLOBAL 0

typedef uint8_t  scope_t;
typedef union _value_t
{
    PS_INTEGER          i;
    PS_UNSIGNED_INTEGER u;
    PS_BOOLEAN          b;
    PS_CHAR             c;
    PS_CHAR             s[PS_STRING_MAX + 1];
    PS_REAL             r;
    void               *p;
} value_t;

typedef struct _symbol_t
{
    char    name[MAX_SYMBOL_NAME + 1];
    kind_t  kind;
    type_t  type;
    scope_t scope;
    size_t  size;
    value_t value;
} symbol_t;

void symbol_normalize_name(char *name);
char *symbol_get_kind_name(kind_t kind);
char *symbol_get_type_name(type_t type);
char *symbol_get_scope_name(scope_t scope);
char *symbol_get_value(symbol_t *symbol);
void symbol_dump(symbol_t *symbol);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_H */
