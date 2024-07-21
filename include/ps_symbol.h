/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_SYMBOL_H
#define _PS_SYMBOL_H

#include "ps_config.h"
#include "ps_error.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef MAX_SYMBOL_NAME
#define MAX_SYMBOL_NAME 31
#endif

    typedef enum e_ps_symbol_type
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
    } __attribute__((__packed__)) ps_symbol_type;

#define PS_SCOPE_GLOBAL 0

    typedef uint8_t ps_scope;
    const ps_scope_max = UINT8_MAX;

    typedef struct _ps_symbol
    {
        char name[MAX_SYMBOL_NAME + 1];
        ps_symbol_type type;
        ps_scope scope;
        ps_value value;
    } ps_symbol;

    typedef uint16_t ps_symbol_key_hash;

    char *ps_symbol_get_type_name(ps_symbol_type type);
    char *symbol_get_scope_name(ps_scope scope);
    char *ps_symbol_dump(ps_symbol *symbol);
    void ps_symbol_debug(ps_symbol *symbol);

    void ps_symbol_normalize_name(ps_symbol *symbol);
    ps_symbol_key_hash ps_symbol_get_hash_key(ps_symbol *symbol);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_H */
