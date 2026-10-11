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
#include "ps_ast_execute.h"
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

static bool ps_ast_execute_procedure_write_or_writeln_or_writestr(ps_interpreter *interpreter,
                                                               const ps_ast_call *procedure_call)
{
    assert(procedure_call != NULL);
    assert(procedure_call->group == PS_AST_STATEMENT);
    assert(procedure_call->kind == PS_AST_PROCEDURE_CALL);
    assert(procedure_call->executable == &ps_system_procedure_write ||
           procedure_call->executable == &ps_system_procedure_writeln ||
           procedure_call->executable == &ps_system_procedure_writestr);

    bool is_writestr = procedure_call->executable == &ps_system_procedure_writestr;

    if (procedure_call->args == NULL || procedure_call->n_args == 0)
    {
        if (procedure_call->executable == &ps_system_procedure_writestr)
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_GENERIC,
                                                    "WRITESTR called without parameters");
        if (procedure_call->executable == &ps_system_procedure_writeln)
            fprintf(stdout, "\n");
        return true;
    }
    ps_string *buffer = NULL;
    if (is_writestr)
    {
        const ps_ast_variable *variable_node = (ps_ast_variable *)procedure_call->args[0];
        ps_value variable_value = {.allocated = false, .type = variable_node->variable->value->type, .data = {0}};
        if (!ps_interpreter_get_variable_value(interpreter, variable_node, &variable_value))
            return false;
        buffer = variable_value.data.s;
        buffer->len = 0;
        memset(buffer->str, '\0', buffer->max + 1);
    }
    for (int i = is_writestr ? 1 : 0; i < procedure_call->n_args; i += 1)
    {
        if (procedure_call->args[i] == NULL)
            continue;
        ps_ast_value arg_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
        int16_t width = procedure_call->formats[i].width;
        int16_t precision = procedure_call->formats[i].precision;
        if (!ps_ast_evaluate_expression(interpreter, procedure_call->args[i], &arg_value))
            return false;
        PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Argument %d: %s", i,
                               ps_value_get_display_string(&arg_value.value, width, precision));
        if (procedure_call->executable == &ps_system_procedure_write &&
            !ps_procedure_write(interpreter, stdout, &arg_value.value, width, precision))
            return false;
        if (procedure_call->executable == &ps_system_procedure_writeln &&
            !ps_procedure_write(interpreter, stdout, &arg_value.value, width, precision))
            return false;
        if (procedure_call->executable == &ps_system_procedure_writestr &&
            !ps_procedure_writestr(interpreter, buffer, &arg_value.value, width, precision))
            return false;
    }
    if (procedure_call->executable == &ps_system_procedure_writeln)
        fprintf(stdout, "\n");
    return true;
}

bool ps_ast_execute_procedure_call_system(ps_interpreter *interpreter, const ps_ast_call *procedure_call)
{
    assert(procedure_call != NULL);
    assert(procedure_call->group == PS_AST_STATEMENT);
    assert(procedure_call->kind == PS_AST_PROCEDURE_CALL);
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "System procedure: %s with %zu argument%s",
                           procedure_call->executable->name, procedure_call->n_args,
                           procedure_call->n_args > 1 ? "s" : "");
    if (procedure_call->executable == &ps_system_procedure_write ||
        procedure_call->executable == &ps_system_procedure_writeln ||
        procedure_call->executable == &ps_system_procedure_writestr)
    {
        // WRITE or WRITELN procedure: Evaluate and output each argument
        return ps_ast_execute_procedure_write_or_writeln_or_writestr(interpreter, procedure_call);
    }
    else if (procedure_call->executable == &ps_system_procedure_randomize)
    {
        // RANDOMIZE procedure (0 or 1 argument)
        PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "System procedure: RANDOMIZE");
        if (procedure_call->n_args == 0)
        {
            return ps_procedure_randomize(interpreter, NULL);
        }
        else if (procedure_call->n_args == 1)
        {
            ps_ast_value arg_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
            if (!ps_ast_evaluate_expression(interpreter, procedure_call->args[0], &arg_value))
                return false;
            PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Argument: %s",
                                   ps_value_get_display_string(&arg_value.value, 0, 0));
            return ps_procedure_randomize(interpreter, &arg_value.value);
        }
        else
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_PARAMETER_COUNT_MISMATCH,
                                                    "RANDOMIZE expects 0 or 1 argument, got %zu",
                                                    procedure_call->n_args);
    }
    else
    {
        ps_interpreter_set_message(interpreter, "%s not implemented yet", procedure_call->executable->name);
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        return false;
    }
}

static bool ps_ast_execute_function_call_system_random(ps_interpreter *interpreter, const ps_ast_call *function_call,
                                                       ps_ast_value *result)
{
    if (function_call->n_args == 0)
    {
        ps_error error = ps_function_random(interpreter, NULL, &result->value);
        if (error != PS_ERROR_NONE)
            return ps_interpreter_set_error_message(interpreter, error, "RANDOM function failed");
        return true;
    }
    if (function_call->n_args == 1)
    {
        ps_ast_value ast_value = {.group = PS_AST_EXPRESSION,
                                  .kind = PS_AST_LITERAL_VALUE,
                                  .value = {.allocated = false, .type = &ps_system_none, .data = {0}}};
        if (!ps_ast_evaluate_expression(interpreter, function_call->args[0], &ast_value))
            return false;
        ps_error error = ps_function_random(interpreter, &ast_value.value, &result->value);
        if (error != PS_ERROR_NONE)
            return ps_interpreter_set_error_message(interpreter, error, "RANDOM(X) function failed");
        return true;
    }
    return ps_interpreter_set_error_message(interpreter, PS_ERROR_TOO_MANY_ARGUMENTS,
                                            "RANDOM function expects 0 or 1 argument");
}

static bool ps_ast_execute_function_call_system_get_tick_count(ps_interpreter *interpreter,
                                                               const ps_ast_call *function_call, ps_ast_value *result)
{
    (void)function_call;
    ps_error error = ps_function_get_tick_count(interpreter, NULL, &result->value);
    if (error != PS_ERROR_NONE)
        return false;
    return true;
}

static bool ps_ast_execute_function_call_system_2arg(ps_interpreter *interpreter, const ps_ast_call *function_call,
                                                     ps_ast_value *result)
{
    assert(function_call != NULL);
    assert(function_call->group == PS_AST_EXPRESSION);
    assert(function_call->kind == PS_AST_FUNCTION_CALL);
    assert(function_call->n_args == 2);
    assert(function_call->executable == &ps_system_function_power ||
           function_call->executable == &ps_system_function_logn);

    ps_ast_value a = {.group = PS_AST_EXPRESSION,
                      .kind = PS_AST_LITERAL_VALUE,
                      .value = {.allocated = false, .type = &ps_system_real, .data = {0}}};

    ps_ast_value b = {.group = PS_AST_EXPRESSION,
                      .kind = PS_AST_LITERAL_VALUE,
                      .value = {.allocated = false, .type = &ps_system_real, .data = {0}}};
    if (!ps_ast_evaluate_expression(interpreter, function_call->args[0], &a))
        return false;
    if (!ps_ast_evaluate_expression(interpreter, function_call->args[1], &b))
        return false;
    ps_error error = function_call->executable == &ps_system_function_power
                         ? ps_function_power(interpreter, &a.value, &b.value, &result->value)
                         : ps_function_logn(interpreter, &a.value, &b.value, &result->value);
    return PS_ERROR_NONE == error;
}

static bool ps_ast_execute_function_call_system_1arg(ps_interpreter *interpreter, const ps_ast_call *function_call,
                                                     ps_ast_value *result)
{
    if (function_call->n_args != 1)
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_TOO_MANY_ARGUMENTS,
                                                "Function %s expects 1 argument", function_call->executable->name);
    // Evaluate argument
    ps_ast_value ast_value = {.group = PS_AST_EXPRESSION,
                              .kind = PS_AST_LITERAL_VALUE,
                              .value = {.allocated = false, .type = &ps_system_none, .data = {0}}};
    if (!ps_ast_evaluate_expression(interpreter, function_call->args[0], &ast_value))
        return false;
    // Call function with argument
    ps_function_1arg_v function = function_call->executable->value->data.x->func_1arg_v;
    ps_error error = function(interpreter, &ast_value.value, &result->value);
    return PS_ERROR_NONE == error;
}

bool ps_ast_execute_function_call_system(ps_interpreter *interpreter, const ps_ast_call *function_call,
                                         ps_ast_value *result)
{
    assert(function_call != NULL);
    assert(function_call->group == PS_AST_EXPRESSION);
    assert(function_call->kind == PS_AST_FUNCTION_CALL);
    assert(function_call->executable->system);

    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "SYSTEM FUNCTION CALL %s", function_call->executable->name);

    // Random can have 0 or 1 argument
    if (function_call->executable == &ps_system_function_random)
        return ps_ast_execute_function_call_system_random(interpreter, function_call, result);

    // GetTickCount has no argument
    if (function_call->executable == &ps_system_function_get_tick_count)
        return ps_ast_execute_function_call_system_get_tick_count(interpreter, function_call, result);

    // Power and logn have 2 arguments
    if (function_call->executable == &ps_system_function_power || function_call->executable == &ps_system_function_logn)
        return ps_ast_execute_function_call_system_2arg(interpreter, function_call, result);

    // All other system functions have 1 argument
    return ps_ast_execute_function_call_system_1arg(interpreter, function_call, result);
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
