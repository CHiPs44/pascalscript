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
#define PS_INTERPRETER_ENVIRONMENTS 8
#endif

#define PS_INTERPRETER_ENVIRONMENT_SYSTEM 0
#define PS_INTERPRETER_ENVIRONMENT_PROGRAM 1

    typedef enum e_ps_interpreter_mode
    {
        MODE_EXEC, /** @brief Execute instructions */
        MODE_SKIP, /** @brief Skip execution but keep declarations */
        MODE_OUT,  /** @brief *FUTURE* Output AST / intermediate language / whatever */
    } ps_interpreter_mode;

    typedef struct s_ps_interpreter
    {
        ps_environment *environments[PS_INTERPRETER_ENVIRONMENTS];
        ps_parser *parser;
        ps_string_heap *string_heap;
        // state
        uint8_t level;
        ps_error error;
        // flags
        bool trace;
        bool debug;
        bool dump;
        // options
        bool range_check; // range checking for integer and real values
        bool bool_eval;   // short circuit boolean evaluation
    } /*__attribute__((__packed__))*/ ps_interpreter;

#define PS_INTERPRETER_SIZEOF sizeof(ps_interpreter)

    /**
     * @brief Initialize interpreter
     * @return NULL if no free memory (errno = ENOMEM) or the interpreter
     */
    ps_interpreter *ps_interpreter_init();

    /** @brief Release interpreter */
    ps_interpreter *ps_interpreter_done(ps_interpreter *interpreter);

    /** @brief Set error & return false */
    bool ps_interpreter_return_error(ps_interpreter *interpreter, ps_error error);

    /** @brief Create a new environment for program, procedure or function */
    bool ps_interpreter_enter_environment(ps_interpreter *interpreter, ps_identifier *name);

    /** @brief Release current environment */
    bool ps_interpreter_exit_environment(ps_interpreter *interpreter);

    /** @brief Get current environment */
    ps_environment *ps_interpreter_get_environment(ps_interpreter *interpreter);

    /** @brief Find symbol by name in current environment or its parents */
    ps_symbol *ps_interpreter_find_symbol(ps_interpreter *interpreter, ps_identifier *name, bool local);

    /** @brief Add symbol to current environment */
    bool ps_interpreter_add_symbol(ps_interpreter *interpreter, ps_symbol *symbol);

    /**
     *  @brief Copy value of "from" into "to", converting unsigned to integer and
     * vice versa, may set error to PS_ERROR_OUT_OF_RANGE or
     * PS_ERROR_TYPE_MISMATCH
     */
    bool ps_interpreter_copy_value(ps_interpreter *interpreter, ps_value *from, ps_value *to);

    // /**
    //  * @brief Allocate new value
    //  * @return NULL if no free memory (errno = ENOMEM)
    //  */
    // ps_value *ps_interpreter_alloc_value(ps_interpreter *interpreter);

    // /** @brief Free existing value */
    // void ps_interpreter_free_value(ps_interpreter *interpreter, ps_value *value);

    // /** @brief Get global symbol */
    // ps_symbol *ps_interpreter_global_get(ps_interpreter *interpreter, char *name);

    // /** @brief Add global symbol */
    // int ps_interpreter_global_add(ps_interpreter *interpreter, ps_symbol *symbol);

    // /** @brief Delete global symbol */
    // int ps_interpreter_global_delete(ps_interpreter *interpreter, char *name);

    bool ps_interpreter_load_string(ps_interpreter *interpreter, char *source, size_t length);

    bool ps_interpreter_load_file(ps_interpreter *interpreter, char *filename);

    bool ps_interpreter_run(ps_interpreter *interpreter, bool exec);

#ifdef __cplusplus
}
#endif

#endif /* _PS_INTERPRETER_H */
