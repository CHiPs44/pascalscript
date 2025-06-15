/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_INTERPRETER_H
#define _PS_INTERPRETER_H

#include <string.h>

#include "ps_environment.h"
#include "ps_error.h"
#include "ps_parser.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct s_ps_interpreter
    {
        ps_environment *environment;
        ps_parser *parser;
        ps_error error;
        // flags
        bool trace;
        bool debug;
        bool from_string; // true if source is a string, false if file
        // options
        bool range_check; // range checking for integer and real values
        bool bool_eval;   // short circuit boolean evaluation
    } ps_interpreter;

#define PS_INTERPRETER_SIZEOF sizeof(ps_interpreter)

    /**
     * @brief Initialize interpreter
     * @return NULL if no free memory (errno = ENOMEM) or the interpreter
     */
    ps_interpreter *ps_interpreter_init();

    /** @brief Release interpreter */
    void ps_interpreter_done(ps_interpreter *interpreter);

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
