/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_ast_execute.h"
#include "ps_execute_statement.h"
#include "ps_ast_execute_expression.h"
#include "ps_interpreter.h"
#include "ps_logger.h"

void ps_ast_debug_execution(ps_interpreter *interpreter, ps_debug_level level, const char *format, ...) // NOSONAR
{
    (void)level; // NOSONAR (unused)
    va_list args;
    va_start(args, format);
    fprintf(interpreter->logger->file, "%*s", interpreter->level * 2, " ");
    vfprintf(interpreter->logger->file, format, args); // NOSONAR
    fprintf(interpreter->logger->file, "\n");
    va_end(args);
}

bool ps_ast_execute_block(ps_interpreter *interpreter, const ps_ast_block *block, bool has_frame)
{
    bool result = false;

    if (!ps_ast_node_check_group((const ps_ast_node *)block, PS_AST_BLOCK))
    {
        ps_interpreter_set_error_message(interpreter, PS_ERROR_UNEXPECTED_AST_NODE, "Expected BLOCK AST node");
        goto error;
    }
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "BLOCK kind=%s name=%s",
                           ps_ast_node_get_kind_name(block->kind), block->name);

    if (!has_frame && !ps_interpreter_enter_frame(interpreter, block))
        goto error;

    result = ps_ast_execute_statement_list(interpreter, block->statement_list);

    if (!has_frame && !ps_interpreter_exit_frame(interpreter))
        goto error;

    return result;

error:
    interpreter->error_line = block->line;
    interpreter->error_column = block->column;
    return false;
}

bool ps_ast_execute_program(ps_interpreter *interpreter, const ps_ast_block *program)
{
    if (!ps_ast_node_check_kind((const ps_ast_node *)program, PS_AST_PROGRAM))
    {
        interpreter->error_line = program->line;
        interpreter->error_column = program->column;
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_UNEXPECTED_AST_NODE, "Expected PROGRAM AST node");
    }
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "PROGRAM %s;", program->name);
    return ps_ast_execute_block(interpreter, program, false);
}

bool ps_ast_execute_procedure(ps_interpreter *interpreter, const ps_ast_block *procedure)
{
    if (!ps_ast_node_check_kind((const ps_ast_node *)procedure, PS_AST_PROCEDURE))
    {
        interpreter->error_line = procedure->line;
        interpreter->error_column = procedure->column;
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_UNEXPECTED_AST_NODE,
                                                "Expected PROCEDURE AST node");
    }
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "PROCEDURE %s;", procedure->name);
    return ps_ast_execute_block(interpreter, procedure, false);
}

bool ps_ast_execute_function(ps_interpreter *interpreter, const ps_ast_block *function)
{
    if (!ps_ast_node_check_kind((const ps_ast_node *)function, PS_AST_FUNCTION))
    {
        interpreter->error_line = function->line;
        interpreter->error_column = function->column;
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_UNEXPECTED_AST_NODE,
                                                "Expected FUNCTION AST node");
    }
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "FUNCTION %s;", function->name);
    return ps_ast_execute_block(interpreter, function, false);
}
