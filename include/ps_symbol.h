/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_error.h"
#include "ps_value.h"

#ifndef _PS_SYMBOL_H
#define _PS_SYMBOL_H

#ifdef __cplusplus
extern "C"
{
#endif

    // Forward reference to value
    typedef struct s_ps_value ps_value;

    /** @brief Only?! 8 kinds of symbols for now... */
    typedef enum e_ps_symbol_kind
    {
        PS_SYMBOL_KIND_AUTO,
        PS_SYMBOL_KIND_TYPE_DEFINITION,
        PS_SYMBOL_KIND_PROGRAM,
        PS_SYMBOL_KIND_UNIT,
        PS_SYMBOL_KIND_CONSTANT,
        PS_SYMBOL_KIND_VARIABLE,
        PS_SYMBOL_KIND_PROCEDURE,
        PS_SYMBOL_KIND_FUNCTION,
        // ...
    } __attribute__((__packed__)) ps_symbol_kind;

    /** @brief Symbol is a named typed value */
    typedef struct s_ps_symbol
    {
        ps_symbol_kind kind;
        bool allocated;
        ps_identifier name;
        ps_value *value; // must be a pointer as it is a forward reference
    } __attribute__((__packed__)) ps_symbol;

#define PS_SYMBOL_KIND_SIZE sizeof(ps_symbol_kind)
#define PS_SYMBOL_SIZE sizeof(ps_symbol)

#define PS_SYMBOL_AUTO_FORMAT "#AUTO_%08X"

    typedef uint32_t ps_symbol_hash_key;

    ps_symbol *ps_symbol_alloc(ps_symbol_kind kind, ps_identifier *name, ps_value *value);
    ps_symbol *ps_symbol_free(ps_symbol *symbol);

    /** @brief Get name of symbol kind */
    char *ps_symbol_get_kind_name(ps_symbol_kind kind);

    /** @brief Get symbol info */
    char *ps_symbol_dump_value(ps_symbol *symbol);

    /** @brief Print symbol info to stderr (if output is NULL) */
    void ps_symbol_debug(FILE *output, char *message, ps_symbol *symbol);

    /** @brief Normalize symbol name (makes it UPPERCASE) in place (no string copy) */
    void ps_symbol_normalize_name(ps_symbol *symbol);

    /** @brief Get hash key of symbol name (using DJB2's algorithm) */
    ps_symbol_hash_key ps_symbol_get_hash_key(char *name);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_H */
