/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_COMPILER_H
#define _PS_COMPILER_H

#include <string.h>

#include "ps_ast.h"
#include "ps_debug.h"
#include "ps_error.h"
#include "ps_parser.h"
#include "ps_string_heap.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct s_ps_compiler
    {
        ps_parser *parser;           /** @brief Parser with lexer(s) with source code buffer(s)                */
        ps_symbol_table *system;     /** @brief Built-in types, constants, variables, procedures and functions */
        ps_string_heap *string_heap; /** @brief String heap to hold string constants                           */
        ps_error error;              /** @brief Current error PS_ERROR_XXX                                     */
        char message[128];           /** @brief Additional error message                                       */
        ps_debug_level debug;        /** @brief Debug level: NONE, TRACE, VERBOSE                              */
    } /*__attribute__((__packed__))*/ ps_compiler;

#define PS_COMPILER_SIZEOF sizeof(ps_compiler)

    /** @brief Initialize compiler and children objects */
    ps_compiler *ps_compiler_alloc(ps_symbol_table *system);

    /** @brief Release compiler and children objects */
    ps_compiler *ps_compiler_free(ps_compiler *compiler);

    /** @brief Set error & return false */
    bool ps_compiler_return_false(ps_compiler *compiler, ps_error error);

    /** @brief Set error & return NULL */
    void *ps_compiler_return_null(ps_compiler *compiler, ps_error error);

    /** @brief Set formatted message & return false */
    bool ps_compiler_set_message(ps_compiler *compiler, const char *format, ...); // NOSONAR

    /** @brief Set error plus formatted message & return false */
    bool ps_compiler_set_error_message(ps_compiler *compiler, ps_error error, const char *format, ...); // NOSONAR

    /** @brief Find symbol by name in current block (or its parents if not local) */
    ps_symbol *ps_compiler_find_symbol(ps_compiler *compiler, ps_ast_block *block, const char *name, bool local);

    /** @brief Add symbol to current block */
    bool ps_compiler_add_symbol(ps_compiler *compiler, ps_ast_block *block, ps_symbol *symbol);

    /** @brief Add variable to current block */
    bool ps_compiler_add_variable(ps_compiler *compiler, ps_ast_block *block, const ps_identifier identifier,
                                  ps_symbol *type_symbol);

    /** @brief Check if current token or value is a number (integer, unsigned, real or integer / unsigned subrange) */
    bool ps_compiler_is_number(ps_compiler *compiler, ps_value *value);

    /** @brief Check if current token or value is an ordinal (integer, unsigned, char or integer / unsigned / char
     * subrange) */
    bool ps_compiler_is_ordinal(ps_compiler *compiler, ps_value *value);

    /**
     *  @brief Copy value of "from" into "to", converting unsigned to integer and vice versa,
     *         may set error to PS_ERROR_OUT_OF_RANGE or PS_ERROR_TYPE_MISMATCH
     */
    bool ps_compiler_copy_value(ps_compiler *compiler, ps_value *from, ps_value *to);

    /** @brief Load source code from string */
    bool ps_compiler_load_string(ps_compiler *compiler, char *source, size_t length);

    /** @brief Load source code from file */
    bool ps_compiler_load_file(ps_compiler *compiler, const char *filename);

    /** @brief Run the compiler on the loaded source code */
    bool ps_compiler_compile(ps_compiler *compiler, ps_ast_block **program);

#ifdef __cplusplus
}
#endif

#endif /* _PS_COMPILER_H */
