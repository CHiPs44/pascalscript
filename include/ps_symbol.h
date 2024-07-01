/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_SYMBOL_H
#define _PS_SYMBOL_H

#include "ps_config.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef MAX_SYMBOL_NAME
#define MAX_SYMBOL_NAME 31
#endif

    typedef enum _ps_kind_t
    {
        KIND_FREE = 0,
        KIND_AUTO,
        KIND_CONSTANT,
        KIND_VARIABLE,
        KIND_PROCEDURE,
        KIND_FUNCTION,
        KIND_TYPE,
        KIND_POINTER,
        KIND_SET,
        KIND_RECORD,
        // ...
    } ps_kind_t;

#define PS_SCOPE_GLOBAL 0

    typedef uint8_t ps_scope_t;
    const ps_scope_max = 255;

    typedef struct _ps_symbol_t
    {
        char name[MAX_SYMBOL_NAME + 1];
        ps_kind_t kind;
        ps_scope_t scope;
        ps_value_t value;
    } ps_symbol_t;

    void ps_symbol_normalize_name(char *name);
    char *ps_symbol_get_kind_name(ps_kind_t kind);
    char *ps_symbol_get_type_name(ps_type_t type);
    char *symbol_get_scope_name(ps_scope_t scope);
    // char *ps_symbol_get_value(ps_symbol_t *symbol);
    void ps_symbol_dump(ps_symbol_t *symbol);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_H */
