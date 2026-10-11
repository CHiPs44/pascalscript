/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_ast_execute.h"
#include "ps_ast_execute_call.h"
#include "ps_ast_execute_expression.h"
#include "ps_execute_statement.h"
#include "ps_ast_execute_system.h"
#include "ps_executable.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_operator.h"
#include "ps_procedures.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_type_definition.h"
#include "ps_value.h"

bool ps_ast_execute_function_call(ps_interpreter *interpreter, const ps_ast_call *function_call, ps_ast_value *result)
{
    assert(function_call != NULL);
    assert(function_call->group == PS_AST_EXPRESSION);
    assert(function_call->kind == PS_AST_FUNCTION_CALL);
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "FUNCTION CALL %s", function_call->executable->name);
    result->value.type = &ps_system_none;
    result->value.data = (ps_value_data){0};
    if (function_call->executable->system)
    {
        return ps_ast_execute_function_call_system(interpreter, function_call, result);
    }
    return ps_interpreter_set_error_message(interpreter, PS_ERROR_NOT_IMPLEMENTED,
                                            "User Function call not implemented");
}

bool ps_ast_execute_procedure_call(ps_interpreter *interpreter, const ps_ast_call *procedure_call)
{
    assert(procedure_call != NULL);
    assert(procedure_call->group == PS_AST_STATEMENT);
    assert(procedure_call->kind == PS_AST_PROCEDURE_CALL);

    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "PROCEDURE CALL %s", procedure_call->executable->name);

    if (procedure_call->executable->system)
    {
        return ps_ast_execute_procedure_call_system(interpreter, procedure_call);
    }

    // Check if argument count is same as procedure declaration
    ps_ast_block *procedure = procedure_call->executable->value->data.x->block;
    if (procedure_call->n_args != procedure->signature->parameter_count)
    {
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_PARAMETER_COUNT_MISMATCH,
                                                "Expected %d arguments, got %d", procedure->signature->parameter_count,
                                                procedure_call->n_args);
    }

    // Evaluate arguments
    ps_value parameters[procedure_call->n_args];
    ps_ast_value arg_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    for (int i = 0; i < procedure_call->n_args; i++)
    {
        if (procedure->signature->parameters[i].byref)
        {
            // TODO initialize byref parameter / argument correctly
            parameters[i].allocated = false;
            parameters[i].type = procedure->signature->parameters[i].type;
            parameters[i].data.h = 0; // ???
        }
        else
        {
            if (!ps_ast_evaluate_expression(interpreter, procedure_call->args[i], &arg_value))
                return false;
            PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Argument: %s",
                                   ps_value_get_display_string(&arg_value.value, 0, 0));
            parameters[i] = arg_value.value;
        }
    }

    // Allocate frame for procedure
    if (!ps_interpreter_enter_frame(interpreter, procedure))
        return false;

    // Store arguments in frame
    ps_symbol *symbol = NULL;
    ps_ast_variable *ast_variable = ps_ast_create_variable_simple(0, 0, procedure, PS_AST_LVALUE, symbol);
    bool ok = true;
    for (int i = 0; i < procedure_call->n_args; i++)
    {
        if (procedure->signature->parameters[i].byref)
            continue;
        symbol = ps_symbol_table_find(procedure->symbols, procedure->signature->parameters[i].name);
        if (symbol == NULL)
        {
            ps_formal_parameter parameter = procedure->signature->parameters[i];
            ps_interpreter_set_error_message(interpreter, PS_ERROR_SYMBOL_NOT_FOUND, "Parameter %s not found",
                                             parameter.name);
            ok = false;
        }
        if (ok)
        {
            ast_variable->variable = symbol;
            if (!ps_interpreter_set_variable_value(interpreter, ast_variable, &parameters[i]))
                ok = false;
        }
        if (!ok)
            goto cleanup;
    }

    // Execute procedure with arguments on top frame of stack
    ok = ps_ast_execute_block(interpreter, procedure, true);
    if (!ps_interpreter_exit_frame(interpreter))
        return false;

    return ok;

cleanup:
    ps_interpreter_exit_frame(interpreter);

    return false;
}
