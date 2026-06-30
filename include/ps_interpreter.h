/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_INTERPRETER_H
#define _PS_INTERPRETER_H

#include <string.h>

#include "ps_ast.h"
#include "ps_debug.h"
#include "ps_error.h"
#include "ps_stack.h"
#include "ps_string_heap.h"
#include "ps_symbol_table.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef PS_INTERPRETER_STACK_SIZE
#define PS_INTERPRETER_STACK_SIZE 1024u
#endif

    typedef struct s_ps_interpreter
    {
        ps_symbol_table *system;     /** @brief Built-in types, constants, variables, procedures and functions */
        ps_string_heap *string_heap; /** @brief String heap to hold string constants (filled by compiler)      */
        ps_stack *stack;             /** @brief Stack to hold variable values                                  */
        char message[128];           /** @brief Explanatory error message                                      */
        uint16_t level;              /** @brief Current environment index : 0 for system, 1 for program, ...   */
        ps_error error;              /** @brief Current error PS_ERROR_XXX                                     */
        ps_debug_level debug : 3;    /** @brief Debug level from FATAL to VERBOSE                              */
        bool range_check : 1;        /** @brief Range checking for integer and real values                     */
        bool bool_eval : 1;          /** @brief *FUTURE* Short circuit boolean evaluation                      */
        bool io_check : 1;           /** @brief *FUTURE* stop or set IOResult on I/O error                     */
    } /*__attribute__((__packed__))*/ ps_interpreter;

#define PS_INTERPRETER_SIZEOF sizeof(ps_interpreter)

    /**
     * @brief Initialize interpreter and children objects
     * @param system      symbol table for system (built-in types, constants, variables, procedures and functions)
     * @param string_heap string heap holding string constants (from compiler)
     * @param range_check enable range checking for values
     * @param bool_eval   *FUTURE* enable short circuit boolean evaluation
     * @param io_check    *FUTURE* enable I/O error checking
     * @return NULL if no free memory (errno = ENOMEM)
     */
    ps_interpreter *ps_interpreter_alloc(ps_symbol_table *system, ps_string_heap *string_heap, bool range_check,
                                         bool bool_eval, bool io_check);

    /** @brief Release interpreter and children objects */
    ps_interpreter *ps_interpreter_free(ps_interpreter *interpreter);

    /** @brief Set error & return false */
    bool ps_interpreter_return_false(ps_interpreter *interpreter, ps_error error);

    /** @brief Set error & return NULL */
    void *ps_interpreter_return_null(ps_interpreter *interpreter, ps_error error);

    /** @brief Set message with format */
    bool ps_interpreter_set_message(ps_interpreter *interpreter, const char *format, ...);

    bool ps_interpreter_enter_frame(ps_interpreter *interpreter, const ps_identifier name, ps_symbol_table *symbols,
                                    size_t n_vars);
    bool ps_interpreter_exit_frame(ps_interpreter *interpreter);

    /** @brief Find symbol by name in current block (or its parents if not local) */
    ps_symbol *ps_interpreter_find_symbol(ps_interpreter *interpreter, ps_ast_block *block, const char *name,
                                          bool local);

    /** @brief Check if current token or value is a number (integer, unsigned, real or integer / unsigned subrange) */
    bool ps_interpreter_is_number(ps_interpreter *interpreter, ps_value *value);

    /** @brief Check if current token or value is an ordinal (integer, unsigned, char or integer / unsigned / char
     * subrange) */
    bool ps_interpreter_is_ordinal(ps_interpreter *interpreter, ps_value *value);

    /**
     *  @brief Copy value of "from" into "to", converting unsigned to integer and vice versa,
     *         may set error to PS_ERROR_OUT_OF_RANGE or PS_ERROR_TYPE_MISMATCH
     */
    bool ps_interpreter_copy_value(ps_interpreter *interpreter, const ps_value *from, ps_value *to);

    /** @brief Run the program */
    bool ps_interpreter_run(ps_interpreter *interpreter, const ps_ast_block *program);

#ifdef __cplusplus
}
#endif

#endif /* _PS_INTERPRETER_H */
