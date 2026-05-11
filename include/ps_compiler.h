/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_COMPILER_H
#define _PS_COMPILER_H

#include <string.h>

#include "ps_environment.h"
#include "ps_error.h"
#include "ps_parser.h"
#include "ps_string_heap.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef PS_COMPILER_ENVIRONMENTS
#define PS_COMPILER_ENVIRONMENTS 1024u
#endif

#define PS_COMPILER_ENVIRONMENT_SYSTEM 0u
#define PS_COMPILER_ENVIRONMENT_PROGRAM 1u

    typedef enum e_ps_compiler_debug
    {
        COMPILER_DEBUG_NONE,    /** @brief No debug */
        COMPILER_DEBUG_TRACE,   /** @brief Output messages to stderr */
        COMPILER_DEBUG_VERBOSE, /** @brief More traces */
    } ps_compiler_debug;

    typedef struct s_ps_compiler
    {
        ps_parser *parser;           /** @brief Parser with lexer(s) with source code buffer(s)              */
        ps_string_heap *string_heap; /** @brief String heap to hold string constants                         */
        uint16_t level;              /** @brief Current environment index : 0 for system, 1 for program, ... */
        ps_error error;              /** @brief Current error PS_ERROR_XXX                                   */
        char message[128];           /** @brief Additional error message                                     */
        ps_compiler_debug debug;     /** @brief Debug level: NONE, TRACE, VERBOSE                            */
        bool range_check;            /** @brief Range checking for integer and real values                   */
        bool bool_eval;              /** @brief *FUTURE* Short circuit boolean evaluation                    */
        bool io_check;               /** @brief *FUTURE* stop or set IOResult on I/O error                   */
        ps_environment
            *environments[PS_COMPILER_ENVIRONMENTS]; /** @brief Environments with enough levels for some recursion */
    } /*__attribute__((__packed__))*/ ps_compiler;

#define PS_COMPILER_SIZEOF sizeof(ps_compiler)

    /**
     * @brief Initialize compiler and children objects
     * @param range_check enable range checking for values
     * @param bool_eval *FUTURE* enable short circuit boolean evaluation
     * @param io_check *FUTURE* enable I/O error checking
     * @return NULL if no free memory (errno = ENOMEM)
     */
    ps_compiler *ps_compiler_alloc(bool range_check, bool bool_eval, bool io_check);

    /** @brief Release compiler and children objects */
    ps_compiler *ps_compiler_free(ps_compiler *compiler);

    /** @brief Set error & return false */
    bool ps_compiler_return_false(ps_compiler *compiler, ps_error error);

    /** @brief Set error & return NULL */
    void *ps_compiler_return_null(ps_compiler *compiler, ps_error error);

    /** @brief Set formatted message */
    bool ps_compiler_set_message(ps_compiler *compiler, char *format, ...);

    /** @brief Create a new environment for program, procedure, function *FUTURE* or unit */
    bool ps_compiler_enter_environment(ps_compiler *compiler, ps_identifier name);

    /** @brief Release current environment */
    bool ps_compiler_exit_environment(ps_compiler *compiler);

    /** @brief Get current environment */
    ps_environment *ps_compiler_get_environment(ps_compiler *compiler);

    /** @brief Find symbol by name in current environment (or its parents if not local) */
    ps_symbol *ps_compiler_find_symbol(ps_compiler *compiler, const char *name, bool local);

    /** @brief Add symbol to current environment */
    bool ps_compiler_add_symbol(ps_compiler *compiler, ps_symbol *symbol);

    /** @brief Add variable to current environment, allocate values for arrays */
    bool ps_compiler_add_variable(ps_compiler *compiler, const ps_identifier identifier, ps_symbol *type_symbol);

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
    bool ps_compiler_run(ps_compiler *compiler);

#ifdef __cplusplus
}
#endif

#endif /* _PS_COMPILER_H */
