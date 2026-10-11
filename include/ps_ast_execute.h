/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_AST_EXECUTE_H
#define _PS_AST_EXECUTE_H

#include "ps_ast.h"
#include <stdarg.h>

// Include submodule headers
#include "ps_execute_statement.h"
#include "ps_ast_execute_expression.h"
#include "ps_ast_execute_system.h"
#include "ps_ast_execute_call.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Debug execution output function */
    void ps_ast_debug_execution(ps_interpreter *interpreter, ps_debug_level level, const char *format, ...);

    // Macro to avoid call and evaluation of arguments
#define PS_AST_DEBUG_EXECUTION(interpreter, level, ...)                                                                \
    if (interpreter->logger->debug_level >= level)                                                                     \
    ps_ast_debug_execution(interpreter, level, __VA_ARGS__)

    /** @brief Execute program */
    bool ps_ast_execute_program(ps_interpreter *interpreter, const ps_ast_block *program);

    /** @brief Execute procedure */
    bool ps_ast_execute_procedure(ps_interpreter *interpreter, const ps_ast_block *procedure);

    /** @brief Execute function */
    bool ps_ast_execute_function(ps_interpreter *interpreter, const ps_ast_block *function);

    /** @brief Execute block (internal) */
    bool ps_ast_execute_block(ps_interpreter *interpreter, const ps_ast_block *block, bool has_frame);

#ifdef __cplusplus
}
#endif

#endif /* _PS_AST_EXECUTE_H */
