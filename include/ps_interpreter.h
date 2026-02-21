/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_INTERPRETER_H
#define _PS_INTERPRETER_H

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

#ifndef PS_INTERPRETER_ENVIRONMENTS
#define PS_INTERPRETER_ENVIRONMENTS 1024u
#endif

#define PS_INTERPRETER_ENVIRONMENT_SYSTEM 0u
#define PS_INTERPRETER_ENVIRONMENT_PROGRAM 1u

    typedef enum e_ps_interpreter_mode
    {
        MODE_EXEC, /** @brief Execute instructions */
        MODE_SKIP, /** @brief Skip execution but parse declarations */
        MODE_CODE, /** @brief *FUTURE* Output AST / intermediate language / whatever */
    } ps_interpreter_mode;

    typedef enum e_ps_interpreter_debug
    {
        DEBUG_NONE,    /** @brief No debug */
        DEBUG_TRACE,   /** @brief Output messages to stderr */
        DEBUG_VERBOSE, /** @brief More traces */
    } ps_interpreter_debug;

    typedef struct s_ps_interpreter
    {
        ps_parser *parser;           /** @brief Parser with lexer(s) with source code buffer(s)              */
        ps_string_heap *string_heap; /** @brief String heap to hold string constants                         */
        uint16_t level;              /** @brief Current environment index : 0 for system, 1 for program, ... */
        ps_error error;              /** @brief Current error PS_ERROR_XXX                                   */
        char message[128];           /** @brief Additional error message                                     */
        ps_interpreter_debug debug;  /** @brief Debug level: NONE, TRACE, VERBOSE                            */
        bool range_check;            /** @brief Range checking for integer and real values                   */
        bool bool_eval;              /** @brief *FUTURE* Short circuit boolean evaluation                    */
        bool io_check;               /** @brief *FUTURE* stop or set IOResult on I/O error                   */
        ps_environment
            *environments[PS_INTERPRETER_ENVIRONMENTS]; /** @brief Environments with enough levels for some recursion */
    } /*__attribute__((__packed__))*/ ps_interpreter;

#define PS_INTERPRETER_SIZEOF sizeof(ps_interpreter)

    /**
     * @brief Initialize interpreter
     * @param range_check enable range checking for integer and real values
     * @param bool_eval enable short circuit boolean evaluation
     * @param io_check enable I/O error checking
     * @return NULL if no free memory (errno = ENOMEM)
     */
    ps_interpreter *ps_interpreter_alloc(bool range_check, bool bool_eval, bool io_check);

    /** @brief Release interpreter */
    ps_interpreter *ps_interpreter_free(ps_interpreter *interpreter);

    /** @brief Set error & return false */
    bool ps_interpreter_return_false(ps_interpreter *interpreter, ps_error error);

    /** @brief Set error & return NULL */
    void *ps_interpreter_return_null(ps_interpreter *interpreter, ps_error error);

    /** @brief Set formatted message */
    bool ps_interpreter_set_message(ps_interpreter *interpreter, const char *format, ...);

    /** @brief Create a new environment for program, procedure, function *FUTURE* or unit */
    bool ps_interpreter_enter_environment(ps_interpreter *interpreter, ps_identifier *name);

    /** @brief Release current environment */
    bool ps_interpreter_exit_environment(ps_interpreter *interpreter);

    /** @brief Get current environment */
    ps_environment *ps_interpreter_get_environment(ps_interpreter *interpreter);

    /** @brief Find symbol by name in current environment (or its parents if not local) */
    ps_symbol *ps_interpreter_find_symbol(ps_interpreter *interpreter, const ps_identifier *name, bool local);

    /** @brief Add symbol to current environment */
    bool ps_interpreter_add_symbol(ps_interpreter *interpreter, ps_symbol *symbol);

    /** @brief Check if current token or value is a number (integer, unsigned, real or integer / unsigned subrange) */
    bool ps_interpreter_is_number(ps_interpreter *interpreter, ps_value *value);
    /** @brief Check if current token or value is an ordinal (integer, unsigned, char or integer / unsigned / char subrange) */
    bool ps_interpreter_is_ordinal(ps_interpreter *interpreter, ps_value *value);

    /**
     *  @brief Copy value of "from" into "to", converting unsigned to integer and vice versa,
     *         may set error to PS_ERROR_OUT_OF_RANGE or PS_ERROR_TYPE_MISMATCH
     */
    bool ps_interpreter_copy_value(ps_interpreter *interpreter, ps_value *from, ps_value *to);

    /** @brief Load source code from string */
    bool ps_interpreter_load_string(ps_interpreter *interpreter, char *source, size_t length);

    /** @brief Load source code from file */
    bool ps_interpreter_load_file(ps_interpreter *interpreter, char *filename);

    /** @brief Run the interpreter on the loaded source code, with option to execute or just parse */
    bool ps_interpreter_run(ps_interpreter *interpreter, bool exec);

#ifdef __cplusplus
}
#endif

#endif /* _PS_INTERPRETER_H */
