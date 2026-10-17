/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_executable.h"
#include "ps_execute.h"
#include "ps_execute_call.h"
#include "ps_execute_expression.h"
#include "ps_execute_statement.h"
#include "ps_execute_system.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_operator.h"
#include "ps_procedures.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_type_definition.h"
#include "ps_value.h"

static bool ps_execute_call(ps_interpreter *interpreter, ps_ast_block *block, const ps_ast_call *call,
                            ps_ast_value *result)
{
    // Check if argument count is same as declaration
    if (call->n_args != block->signature->parameter_count)
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_PARAMETER_COUNT_MISMATCH,
                                                "Expected %d arguments, got %d", block->signature->parameter_count,
                                                call->n_args);

    // Evaluate arguments
    ps_value arguments[call->n_args];
    ps_ast_value arg_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    for (int i = 0; i < call->n_args; i++)
    {
        if (block->signature->parameters[i].byref)
        {
            // TODO initialize byref parameter / argument correctly
            arguments[i].allocated = false;
            arguments[i].type = block->signature->parameters[i].type;
            arguments[i].data.h = 0; // ???
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_NOT_IMPLEMENTED,
                                                    "Initialization of byref parameters not implemented");
        }
        else
        {
            if (!ps_evaluate_expression(interpreter, call->args[i], &arg_value))
                return false;
            PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Argument %d: %s", i,
                                   ps_value_get_display_string(&arg_value.value, 0, 0));
            arguments[i] = arg_value.value;
        }
    }

    // Allocate frame for procedure
    if (!ps_interpreter_enter_frame(interpreter, block))
        return false;

    // Store arguments in frame
    ps_symbol *symbol = NULL;
    ps_ast_variable *ast_variable = ps_ast_create_variable_simple(0, 0, block, PS_AST_LVALUE, symbol);
    bool ok = true;
    for (int i = 0; i < call->n_args; i++)
    {
        if (block->signature->parameters[i].byref)
            continue;
        symbol = ps_symbol_table_find(block->symbols, block->signature->parameters[i].name);
        if (symbol == NULL)
        {
            ps_formal_parameter parameter = block->signature->parameters[i];
            ps_interpreter_set_error_message(interpreter, PS_ERROR_SYMBOL_NOT_FOUND, "Parameter %s not found",
                                             parameter.name);
            ok = false;
        }
        if (ok)
        {
            ast_variable->variable = symbol;
            if (!ps_interpreter_set_variable_value(interpreter, ast_variable, &arguments[i]))
                ok = false;
        }
        if (!ok)
            goto cleanup;
    }

    // Execute block with arguments on top frame of stack
    ok = ps_execute_block(interpreter, block, true);

    // Get RESULT variable value into result value
    if (result != NULL)
    {
        ps_symbol *result_symbol = ps_symbol_table_find(block->symbols, "RESULT");
        if (result_symbol == NULL)
            goto cleanup;
        ps_ast_variable result_variable = {0};
        result_variable.owner = block;
        result_variable.variable = result_symbol;
        result_variable.group = PS_AST_GROUP_LVALUE;
        result_variable.kind = PS_AST_LVALUE;
        result_variable.dimensions = 0;
        ps_value result_value = {.allocated = false, .type = block->signature->result_type, .data = {0}};
        if (!ps_interpreter_get_variable_value(interpreter, &result_variable, &result_value))
            goto cleanup;
    }

    // Release frame
    if (!ps_interpreter_exit_frame(interpreter))
        return false;

    return ok;

cleanup:
    ps_interpreter_exit_frame(interpreter);

    return false;
}

bool ps_execute_function_call(ps_interpreter *interpreter, const ps_ast_call *function_call, ps_ast_value *result)
{
    assert(function_call != NULL);
    assert(function_call->group == PS_AST_EXPRESSION);
    assert(function_call->kind == PS_AST_FUNCTION_CALL);

    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "FUNCTION CALL %s", function_call->executable->name);

    result->value.type = &ps_system_none;
    result->value.data = (ps_value_data){0};

    if (function_call->executable->system)
        return ps_execute_function_call_system(interpreter, function_call, result);

    ps_ast_block *function = function_call->executable->value->data.x->block;

    if (!ps_execute_call(interpreter, function, function_call, result))
        return false;

    return true;
}

bool ps_execute_procedure_call(ps_interpreter *interpreter, const ps_ast_call *procedure_call)
{
    assert(procedure_call != NULL);
    assert(procedure_call->group == PS_AST_STATEMENT);
    assert(procedure_call->kind == PS_AST_PROCEDURE_CALL);

    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "PROCEDURE CALL %s", procedure_call->executable->name);

    if (procedure_call->executable->system)
    {
        return ps_execute_procedure_call_system(interpreter, procedure_call);
    }

    ps_ast_block *procedure = procedure_call->executable->value->data.x->block;

    return ps_execute_call(interpreter, procedure, procedure_call, NULL);
}
