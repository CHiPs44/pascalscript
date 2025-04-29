/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_system_types.h"
#include "ps_error.h"

#ifndef _PS_SYMBOL_H
#define _PS_SYMBOL_H

#ifdef __cplusplus
extern "C"
{
#endif

    // Forward reference to value
    typedef struct s_ps_value ps_value;

#define PS_SYMBOL_SCOPE_NAME_LEN 15
#define PS_SYMBOL_SCOPE_NAME_SIZE (PS_SYMBOL_SCOPE_NAME_LEN + 1)
#define PS_SYMBOL_SCOPE_SYSTEM_NAME "SYSTEM"
#define PS_SYMBOL_SCOPE_GLOBAL_NAME "GLOBAL"
#define PS_SYMBOL_SCOPE_LOCAL_FORMAT "L%06d"
#define PS_SYMBOL_SCOPE_UNIT_FORMAT "U%06d"

    typedef enum e_ps_symbol_scope
    {
        PS_SYMBOL_SCOPE_NONE = 0,   /** @brief No scope */
        PS_SYMBOL_SCOPE_SYSTEM,     /** @brief System defined: for things like MaxInt, False, True, StdOut, StdErr, ... */
        PS_SYMBOL_SCOPE_GLOBAL,     /** @brief For constants, types, variables, procedures and functions at top level */
        PS_SYMBOL_SCOPE_UNIT,       /** @brief Same for units, 29 max */
        PS_SYMBOL_SCOPE_LOCAL = 64, /** @brief Same for local levels */
        PS_SYMBOL_SCOPE_MAX = 255   /** @brief 96 levels of imbrication should be enough */
    } __attribute__((__packed__)) ps_symbol_scope;

#define PS_SYMBOL_AUTO_FORMAT "#AUTO_%04X"

    typedef enum e_ps_symbol_kind
    {
        PS_SYMBOL_KIND_AUTO,
        PS_SYMBOL_KIND_PROGRAM,
        PS_SYMBOL_KIND_CONSTANT,
        PS_SYMBOL_KIND_VARIABLE,
        PS_SYMBOL_KIND_TYPE_DEFINITION,
        PS_SYMBOL_KIND_PROCEDURE,
        PS_SYMBOL_KIND_FUNCTION,
        PS_SYMBOL_KIND_UNIT,
        // ...
        PS_SYMBOL_KIND_MAX = 15
    } __attribute__((__packed__)) ps_symbol_kind;

    /** @brief Symbol is a named typed value within a scope */
    typedef struct s_ps_symbol
    {
        ps_symbol_scope scope : 8;
        ps_symbol_kind kind : 4;
        bool allocated : 1;
        uint8_t reserved : 3;
        ps_identifier name;
        ps_value *value; // must be a pointer as it is a forward reference
    } __attribute__((__packed__)) ps_symbol;

#define PS_SYMBOL_SCOPE_SIZE sizeof(ps_symbol_scope)
#define PS_SYMBOL_KIND_SIZE sizeof(ps_symbol_kind)
#define PS_SYMBOL_SIZE sizeof(ps_symbol)

    ps_symbol *ps_symbol_init(ps_symbol_scope scope, ps_symbol_kind kind, ps_identifier *name, ps_value *value);
    void ps_symbol_free(ps_symbol *symbol);

    /** @brief Get name of symbol scope */
    char *ps_symbol_get_scope_name(ps_symbol_scope scope);

    /** @brief Get name of symbol kind */
    char *ps_symbol_get_kind_name(ps_symbol_kind kind);

    /** @brief Get symbol info */
    char *ps_symbol_dump_value(ps_symbol *symbol);

    /** @brief Print symbol info to stderr (if output is NULL) */
    void ps_symbol_debug(FILE *output, char *message, ps_symbol *symbol);

    /** @brief Normalize symbol name (makes it UPPERCASE) in place (no string copy) */
    void ps_symbol_normalize_name(ps_symbol *symbol);

    // typedef uint16_t ps_symbol_hash_key;

    // /** @brief Get hash key of symbol name (using DJB2's algorithm) */
    // ps_symbol_hash_key ps_symbol_get_hash_key(ps_symbol *symbol);

    bool ps_symbol_scope_is_unit(ps_symbol_scope scope);
    bool ps_symbol_scope_is_local(ps_symbol_scope scope);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_H */
