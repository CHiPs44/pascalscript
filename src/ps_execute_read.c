/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <stdio.h>

#include "ps_ast.h"
#include "ps_execute_read.h"
#include "ps_interpreter.h"
#include "ps_procedures.h"
#include "ps_string.h"
#include "ps_system.h"
#include "ps_value.h"

static bool ps_execute_procedure_read_argument(ps_interpreter *interpreter, const ps_ast_call *procedure_call, int i)
{
    const ps_ast_variable *variable_node = (const ps_ast_variable *)procedure_call->args[i];
    if (variable_node == NULL || variable_node->kind != PS_AST_LVALUE)
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_EXPECTED_VARIABLE,
                                                "Read argument %d must be a variable", i);

    ps_value value = {.allocated = false,
                      .type = variable_node->variable->value->type,
                      .data = {0}};
    if (!ps_procedure_read(interpreter, stdin, &value))
        return false;
    if (!ps_interpreter_set_variable_value(interpreter, variable_node, &value))
        return false;
    if (ps_value_is_string(&value))
        ps_string_free(value.data.s);
    return true;
}

static bool ps_execute_procedure_read_or_readln(ps_interpreter *interpreter, const ps_ast_call *procedure_call)
{
    if (procedure_call->n_args == 0)
    {
        if (procedure_call->executable == &ps_system_procedure_readln)
        {
            int c;
            do
            {
                c = fgetc(stdin);
            } while (c != EOF && c != '\n');
        }
        return true;
    }

    for (int i = 0; i < procedure_call->n_args; i += 1)
    {
        if (!ps_execute_procedure_read_argument(interpreter, procedure_call, i))
            return false;
    }

    if (procedure_call->executable == &ps_system_procedure_readln)
    {
        int c;
        do
        {
            c = fgetc(stdin);
        } while (c != EOF && c != '\n');
    }
    return true;
}

static bool ps_execute_procedure_readstr(ps_interpreter *interpreter, const ps_ast_call *procedure_call)
{
    if (procedure_call->n_args < 2)
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_PARAMETER_COUNT_MISMATCH,
                                                "ReadStr expects a string and at least one variable");

    ps_value source = {.allocated = false, .type = &ps_system_string, .data = {0}};
    const ps_ast_variable *source_node = (const ps_ast_variable *)procedure_call->args[0];
    if (source_node == NULL || source_node->kind != PS_AST_LVALUE)
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_EXPECTED_VARIABLE,
                                                "ReadStr source must be a string variable");
    if (!ps_interpreter_get_variable_value(interpreter, source_node, &source))
        return false;

    ps_string *buffer = ps_string_create(source.data.s == NULL ? "" : (char *)source.data.s->str);
    if (buffer == NULL)
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_OUT_OF_MEMORY,
                                                "Could not allocate ReadStr buffer");

    for (int i = 1; i < procedure_call->n_args; i += 1)
    {
        const ps_ast_variable *variable_node = (const ps_ast_variable *)procedure_call->args[i];
        if (variable_node == NULL || variable_node->kind != PS_AST_LVALUE)
        {
            ps_string_free(buffer);
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_EXPECTED_VARIABLE,
                                                    "ReadStr argument %d must be a variable", i);
        }
        ps_value value = {.allocated = false,
                          .type = variable_node->variable->value->type,
                          .data = {0}};
        if (!ps_procedure_readstr(interpreter, buffer, &value))
        {
            ps_string_free(buffer);
            return false;
        }
        if (!ps_interpreter_set_variable_value(interpreter, variable_node, &value))
        {
            ps_string_free(buffer);
            if (ps_value_is_string(&value))
                ps_string_free(value.data.s);
            return false;
        }
        if (ps_value_is_string(&value))
            ps_string_free(value.data.s);
    }

    ps_string_free(buffer);
    return true;
}

bool ps_execute_procedure_read_call(ps_interpreter *interpreter, const ps_ast_call *procedure_call)
{
    assert(procedure_call != NULL);
    assert(procedure_call->group == PS_AST_STATEMENT);
    assert(procedure_call->kind == PS_AST_PROCEDURE_CALL);

    if (procedure_call->executable == &ps_system_procedure_read ||
        procedure_call->executable == &ps_system_procedure_readln)
        return ps_execute_procedure_read_or_readln(interpreter, procedure_call);
    if (procedure_call->executable == &ps_system_procedure_readstr)
        return ps_execute_procedure_readstr(interpreter, procedure_call);

    return ps_interpreter_set_error_message(interpreter, PS_ERROR_UNEXPECTED_AST_NODE,
                                            "Unexpected procedure in Read executor");
}
