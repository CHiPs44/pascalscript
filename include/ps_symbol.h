/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_SYMBOL_H
#define _PS_SYMBOL_H

#include "ps_types.h"
#include "ps_error.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum e_ps_symbol_kind
    {
        PS_SYMBOL_KIND_FREE = 0,
        PS_SYMBOL_KIND_AUTO,
        PS_SYMBOL_KIND_CONSTANT,
        PS_SYMBOL_KIND_VARIABLE,
        // PS_SYMBOL_KIND_PROCEDURE,
        // PS_SYMBOL_KIND_FUNCTION,
        // PS_SYMBOL_KIND_TYPE,
        // PS_SYMBOL_KIND_POINTER,
        // PS_SYMBOL_KIND_SET,
        // PS_SYMBOL_KIND_RECORD,
        // ...
    } __attribute__((__packed__)) ps_symbol_kind;

#define PS_SCOPE_SYSTEM 0
#define PS_SCOPE_GLOBAL 1
#define PS_SCOPE_LOCAL_START 2
#define PS_SCOPE_LOCAL_FORMAT "LOCAL%03d"

    typedef uint8_t ps_scope;
    const ps_scope_max = UINT8_MAX;

    typedef struct s_ps_symbol
    {
        ps_symbol_kind kind;
        ps_scope scope;
        char name[PS_IDENTIFIER_MAX + 1];
        ps_value value;
    } ps_symbol;

    /** @brief Get kind name for symbol () */
    char *ps_symbol_get_kind_name(ps_symbol_kind kind);

    /** @brief Get kind name for symbol () */
    char *ps_symbol_get_scope_name(ps_scope scope);

    /** @brief Get symbol info */
    char *ps_symbol_dump(ps_symbol *symbol);

    /** @brief Print symbol info to stderr */
    void ps_symbol_debug(ps_symbol *symbol);

    /** @brief Normalize symbol name (=> UPPERCASE) in place (no string copy) */
    void ps_symbol_normalize_name(ps_symbol *symbol);

    // typedef uint16_t ps_symbol_hash_key;

    // /** @brief Get hash key of symbol name (using DJB2's algorithm) */
    // ps_symbol_hash_key ps_symbol_get_hash_key(ps_symbol *symbol);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_H */
