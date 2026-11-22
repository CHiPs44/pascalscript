/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_execute.h"
#include "ps_execute_expression.h"
#include "ps_execute_write.h"
#include "ps_interpreter.h"
#include "ps_procedures.h"
#include "ps_string.h"
#include "ps_system.h"
#include "ps_value.h"

static bool ps_execute_procedure_write_or_writeln_or_writestr_without_arguments(ps_interpreter *interpreter,
                                                                                const ps_ast_call *procedure_call)
{
    if (procedure_call->executable == &ps_system_procedure_writestr)
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_GENERIC, "WRITESTR called without parameters");
    if (procedure_call->executable == &ps_system_procedure_writeln)
        fprintf(stdout, "\n");
    return true;
}

static bool ps_execute_procedure_writestr_initialize(ps_interpreter *interpreter, const ps_ast_call *procedure_call,
                                                     ps_string **buffer)
{
    const ps_ast_variable *variable_node = (ps_ast_variable *)procedure_call->args[0];
    ps_value variable_value = {.allocated = false, .type = variable_node->variable->value->type, .data = {0}};
    if (!ps_interpreter_get_variable_value(interpreter, variable_node, &variable_value))
        return false;
    *buffer = variable_value.data.s;
    (*buffer)->len = 0;
    memset((*buffer)->str, '\0', (*buffer)->max + 1);
    return true;
}

static bool ps_execute_procedure_write_or_writeln_or_writestr_argument(ps_interpreter *interpreter,
                                                                       const ps_ast_call *procedure_call, int i,
                                                                       ps_string *buffer)
{
    if (procedure_call->args[i] == NULL)
        return true;
    ps_ast_value arg_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    int16_t width = procedure_call->formats == NULL ? 0 : procedure_call->formats[i].width;
    int16_t precision = procedure_call->formats == NULL ? 0 : procedure_call->formats[i].precision;
    if (!ps_evaluate_expression(interpreter, procedure_call->args[i], &arg_value))
        return false;
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Argument %d: %s", i,
                           ps_value_get_display_string(&arg_value.value, width, precision));
    if (procedure_call->executable == &ps_system_procedure_write)
        return ps_procedure_write(interpreter, stdout, &arg_value.value, width, precision);
    if (procedure_call->executable == &ps_system_procedure_writeln)
        return ps_procedure_write(interpreter, stdout, &arg_value.value, width, precision);
    return ps_procedure_writestr(interpreter, buffer, &arg_value.value, width, precision);
}

bool ps_execute_procedure_write_call(ps_interpreter *interpreter, const ps_ast_call *procedure_call)
{
    assert(procedure_call != NULL);
    assert(procedure_call->group == PS_AST_STATEMENT);
    assert(procedure_call->kind == PS_AST_PROCEDURE_CALL);
    assert(procedure_call->executable == &ps_system_procedure_write ||
           procedure_call->executable == &ps_system_procedure_writeln ||
           procedure_call->executable == &ps_system_procedure_writestr);

    if (procedure_call->args == NULL || procedure_call->n_args == 0)
        return ps_execute_procedure_write_or_writeln_or_writestr_without_arguments(interpreter, procedure_call);

    bool is_writestr = procedure_call->executable == &ps_system_procedure_writestr;
    ps_string *buffer = NULL;

    if (is_writestr && !ps_execute_procedure_writestr_initialize(interpreter, procedure_call, &buffer))
        return false;

    for (int i = is_writestr ? 1 : 0; i < procedure_call->n_args; i += 1)
    {
        if (!ps_execute_procedure_write_or_writeln_or_writestr_argument(interpreter, procedure_call, i, buffer))
            return false;
    }
    if (procedure_call->executable == &ps_system_procedure_writeln)
        fprintf(stdout, "\n");
    return true;
}
