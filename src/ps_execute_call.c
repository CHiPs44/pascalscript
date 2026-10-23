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
    bool ok = true;
    bool frame_entered = false;
    ps_symbol *symbol = NULL;
    ps_ast_variable *ast_variable = NULL;
    ps_ast_variable result_variable = {0};
    ps_value arguments[PS_PARAMETERS_MAX] = {0};
    ps_ast_value arg_value = {.line = call->line,
                              .column = call->column,
                              .group = PS_AST_EXPRESSION,
                              .kind = PS_AST_LITERAL_VALUE,
                              .flags = 0,
                              .value = {.allocated = false, .type = &ps_system_none, .data = {0}}};

    // Check if argument count is same as declaration
    if (call->n_args != block->signature->parameter_count)
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_PARAMETER_COUNT_MISMATCH,
                                                "Expected %d arguments, got %d in call of %s",
                                                block->signature->parameter_count, call->n_args, block->name);

    ast_variable = ps_ast_create_variable_simple(call->line, call->column, block, PS_AST_LVALUE, symbol);
    if (ast_variable == NULL)
    {
        ps_interpreter_set_error_message(interpreter, PS_ERROR_OUT_OF_MEMORY, "Could not allocate AST variable");
        goto cleanup;
    }

    // Evaluate arguments
    for (int i = 0; i < call->n_args; i++)
    {
        if (block->signature->parameters[i].byref)
        {
            // For by-reference parameters, the argument must be a variable (lvalue)
            if (call->args[i]->group != PS_AST_GROUP_LVALUE)
            {
                ps_interpreter_set_error_message(interpreter, PS_ERROR_EXPECTED_VARIABLE,
                                                 "Argument %d must be a variable for by-reference parameter %s", i,
                                                 block->signature->parameters[i].name);
                ok = false;
            }
            else
            {
                // empty value, the actual value will be set in the frame by reference
                arguments[i] = (ps_value){.allocated = false, .type = &ps_system_none, .data = {0}};
            }
        }
        else
        {
            if (!ps_evaluate_expression(interpreter, call->args[i], &arg_value))
                ok = false;
            else
            {
                PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Argument %d: %s", i,
                                       ps_value_get_display_string(&arg_value.value, 0, 0));
                arguments[i] = arg_value.value;
            }
        }
        if (!ok)
            goto cleanup;
    }

    // Allocate frame for procedure
    if (!ps_interpreter_enter_frame(interpreter, block))
        return false;
    frame_entered = true;

    // Store argument in frame
    for (int i = 0; i < call->n_args; i++)
    {
        symbol = ps_symbol_table_find(block->symbols, block->signature->parameters[i].name);
        if (symbol == NULL)
        {
            ps_formal_parameter parameter = block->signature->parameters[i];
            ps_interpreter_set_error_message(interpreter, PS_ERROR_SYMBOL_NOT_FOUND, "Parameter %s not found",
                                             parameter.name);
            ok = false;
        }
        if (ok && block->signature->parameters[i].byref)
        {
            ps_ast_variable parameter_variable = {.owner = block, .variable = symbol};
            const ps_ast_variable *argument_variable = (const ps_ast_variable *)call->args[i];
            if (!ps_interpreter_bind_variable_reference(interpreter, &parameter_variable, argument_variable))
                ok = false;
        }
        else if (ok)
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

    // For functions, copy RESULT variable value into result value
    if (ok && result != NULL)
    {
        ps_symbol *result_symbol = ps_symbol_table_find(block->symbols, "RESULT");
        if (result_symbol == NULL)
        {
            ps_interpreter_set_error_message(interpreter, PS_ERROR_SYMBOL_NOT_FOUND, "RESULT variable not found");
            ok = false;
            goto cleanup;
        }
        result_variable.owner = block;
        result_variable.variable = result_symbol;
        result_variable.group = PS_AST_GROUP_LVALUE;
        result_variable.kind = PS_AST_LVALUE;
        result_variable.dimensions = 0;
        ps_value result_value = {.allocated = false, .type = block->signature->result_type, .data = {0}};
        if (!ps_interpreter_get_variable_value(interpreter, &result_variable, &result_value))
        {
            // ps_interpreter_set_error_message(interpreter, PS_ERROR_GENERIC, "Could not get RESULT variable value");
            ok = false;
            goto cleanup;
        }
        if (!ps_interpreter_copy_value(interpreter, &result_value, &result->value))
        {
            // ps_interpreter_set_error_message(interpreter, PS_ERROR_GENERIC, "Could not copy RESULT variable value");
            ok = false;
            goto cleanup;
        }
        if (ps_value_is_string(&result_value))
        {
            const ps_string *source = result_value.data.s;
            result->value.data.s = ps_string_heap_create(interpreter->string_heap, (const char *)source->str);
            if (result->value.data.s == NULL)
            {
                ps_interpreter_set_error_message(interpreter, PS_ERROR_OUT_OF_MEMORY,
                                                 "Could not retain string result from %s", block->name);
                ok = false;
                goto cleanup;
            }
            result->value.string_heap_owned = true;
        }
    }

cleanup:
    if (ast_variable != NULL)
        ps_ast_free_variable(ast_variable);
    if (frame_entered)
        ps_interpreter_exit_frame(interpreter);

    return ok;
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
