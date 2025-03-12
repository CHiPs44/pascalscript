/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include "ps_system_types.h"
#include "ps_error.h"

#ifndef _PS_SYMBOL_H
#define _PS_SYMBOL_H

#ifdef __cplusplus
extern "C"
{
#endif

    // Forward reference
    typedef struct s_ps_value ps_value;

#define PS_SYMBOL_SCOPE_NAME_LEN 7
#define PS_SYMBOL_SCOPE_NAME_SIZE (PS_SYMBOL_SCOPE_NAME_LEN + 1)
#define PS_SYMBOL_SCOPE_SYSTEM_NAME "SYSTEM"
#define PS_SYMBOL_SCOPE_GLOBAL_NAME "GLOBAL"
#define PS_SYMBOL_SCOPE_LOCAL_FORMAT "L%06d"
#define PS_SYMBOL_SCOPE_LOCAL_FORMAT "U%06d"

    typedef enum e_ps_symbol_scope
    {
        PS_SYMBOL_SCOPE_SYSTEM = 0,                  /** @brief System defined: for things like StdOut, StdErr, False, True, ... */
        PS_SYMBOL_SCOPE_GLOBAL,                      /** @brief User defined: for constants, types, variables, procedures and functions at top level */
        PS_SYMBOL_SCOPE_LOCAL,                       /** @brief Local defined */
        PS_SYMBOL_SCOPE_UNIT = (UINT16_MAX + 1) / 2, /** @brief Unit defined */
        PS_SYMBOL_SCOPE_MAX = UINT16_MAX             /** @brief Ensure scope "packs" to a 16 bits unsigned */
    } __attribute__((__packed__)) ps_symbol_scope;

#define PS_SYMBOL_KIND_NAME_LEN 15
#define PS_SYMBOL_KIND_NAME_SIZE (PS_SYMBOL_KIND_NAME_LEN + 1)
#define PS_SYMBOL_AUTO_FORMAT "#AUTO_%04X"

    typedef enum e_ps_symbol_kind
    {
        PS_SYMBOL_KIND_FREE = 0,
        PS_SYMBOL_KIND_AUTO,
        PS_SYMBOL_KIND_CONSTANT,
        PS_SYMBOL_KIND_VARIABLE,
        PS_SYMBOL_KIND_TYPE_DEFINITION,
        PS_SYMBOL_KIND_PROCEDURE,
        PS_SYMBOL_KIND_FUNCTION,
        PS_SYMBOL_KIND_UNIT,
        // ...
        PS_SYMBOL_KIND_MAX = UINT16_MAX /** @brief Ensure kind "packs" to a 16 bits unsigned */
    } __attribute__((__packed__)) ps_symbol_kind;

    /** @brief Symbol is a named typed value within a scope */
    typedef struct s_ps_symbol
    {
        ps_symbol_scope scope;
        ps_symbol_kind kind;
        ps_identifier name;
        ps_value *value;
    } __attribute__((__packed__)) ps_symbol;

#define PS_SYMBOL_SCOPE_SIZE sizeof(ps_symbol_scope)
#define PS_SYMBOL_KIND_SIZE sizeof(ps_symbol_kind)
#define PS_SYMBOL_SIZE sizeof(ps_symbol)

    /** @brief Get kind name for symbol */
    char *ps_symbol_get_kind_name(ps_symbol_kind kind);

    /** @brief Get scope name for symbol */
    char *ps_symbol_get_scope_name(ps_symbol_scope scope);

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
