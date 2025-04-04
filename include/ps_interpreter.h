/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_INTERPRETER_H
#define _PS_INTERPRETER_H

#include <string.h>

#include "ps_error.h"
#include "ps_parser.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    // static ps_error interpreter_errno = PS_ERROR_ZERO;

    typedef struct s_ps_interpreter
    {
        ps_parser *parser;
        ps_error error;
        bool trace;
        bool debug;
        bool range_check;
        bool allocated;
    } ps_interpreter;

    ps_interpreter *ps_interpreter_init(ps_interpreter *interpreter);
    void ps_interpreter_done(ps_interpreter *interpreter);

    /**
     * @brief Allocate new value
     * @return NULL if no free memory (errno = ENOMEM)
     */
    ps_value *ps_interpreter_alloc_value(ps_interpreter *interpreter);

    /** @brief Free existing value */
    void ps_interpreter_free_value(ps_interpreter *interpreter, ps_value *value);

    /** @brief Get global symbol */
    ps_symbol *ps_interpreter_global_get(ps_interpreter *interpreter, char *name);

    /** @brief Add global symbol */
    int ps_interpreter_global_add(ps_interpreter *interpreter, ps_symbol *symbol);

    /** @brief Delete global symbol */
    int ps_interpreter_global_delete(ps_interpreter *interpreter, char *name);

    // ps_symbol *runtime_auto_add_integer(ps_interpreter *interpreter, int value);
    // int ps_interpreter_auto_gc(ps_interpreter *interpreter);
    bool ps_interpreter_load_string(ps_interpreter *interpreter, char *source, size_t length);

    bool ps_interpreter_run(ps_interpreter *interpreter);

    bool ps_visit_start(ps_interpreter *interpreter);

#ifdef __cplusplus
}
#endif

#endif /* _PS_INTERPRETER_H */
