/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <stdio.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_ast_execute.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_logger.h"
#include "ps_memory.h"
#include "ps_operator.h"
#include "ps_procedures.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_value.h"

void ps_ast_debug_execute(ps_interpreter *interpreter, ps_debug_level level, const char *format, ...) // NOSONAR
{
    if (interpreter->logger->debug_level >= level)
    {
        va_list args;
        va_start(args, format);
        fprintf(interpreter->logger->file, "%*s", interpreter->level * 2, " ");
        vfprintf(interpreter->logger->file, format, args); // NOSONAR
        fprintf(interpreter->logger->file, "\n");
        va_end(args);
    }
}

bool ps_ast_execute_block(ps_interpreter *interpreter, const ps_ast_block *block)
{
    bool result = false;
    if (!ps_ast_node_check_group((const ps_ast_node *)block, PS_AST_BLOCK))
        return false;
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "BLOCK kind=%s name=%s", ps_ast_node_get_kind_name(block->kind),
                         block->name);
    if (!ps_interpreter_enter_frame(interpreter, (ps_ast_block *)block))
        return false;
    result = ps_ast_execute_statement_list(interpreter, block->statement_list);
    if (!ps_interpreter_exit_frame(interpreter))
        return false;
    return result;
}

bool ps_ast_execute_program(ps_interpreter *interpreter, const ps_ast_block *program)
{
    if (!ps_ast_node_check_kind((const ps_ast_node *)program, PS_AST_PROGRAM))
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_UNEXPECTED_AST_NODE, "Expected PROGRAM AST node");
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "PROGRAM %s;", program->name);
    return ps_ast_execute_block(interpreter, program);
}

bool ps_ast_execute_procedure(ps_interpreter *interpreter, const ps_ast_block *procedure)
{
    if (!ps_ast_node_check_kind((const ps_ast_node *)procedure, PS_AST_PROCEDURE))
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_UNEXPECTED_AST_NODE,
                                                "Expected PROCEDURE AST node");
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "PROCEDURE %s;", procedure->name);
    return ps_ast_execute_block(interpreter, procedure);
}

bool ps_ast_execute_function(ps_interpreter *interpreter, const ps_ast_block *function)
{
    if (!ps_ast_node_check_kind((const ps_ast_node *)function, PS_AST_FUNCTION))
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_UNEXPECTED_AST_NODE,
                                                "Expected FUNCTION AST node");
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "FUNCTION %s;", function->name);
    return ps_ast_execute_block(interpreter, function);
}

bool ps_ast_execute_statement_list(ps_interpreter *interpreter, const ps_ast_statement_list *statement_list)
{
    if (statement_list == NULL)
        return true; // Empty statement list is valid (no-op)
    if (!ps_ast_node_check_kind((const ps_ast_node *)statement_list, PS_AST_STATEMENT_LIST))
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_UNEXPECTED_AST_NODE,
                                                "Expected STATEMENT_LIST AST node");
    if (statement_list->count == 0)
        return true; // Empty statement list is valid (no-op)
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "STATEMENT_LIST %zu:", statement_list->count);
    for (size_t i = 0; i < statement_list->count; i++)
    {
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "STATEMENT %zu/%zu:", i + 1, statement_list->count);
        assert(statement_list->statements != NULL);
        assert(statement_list->statements[i] != NULL);
        if (!ps_ast_execute_statement(interpreter, statement_list->statements[i]))
        {
            ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "STATEMENT %zu failed", i + 1);
            return false;
        }
    }
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "STATEMENT_LIST completed");
    return true;
}

bool ps_ast_execute_statement(ps_interpreter *interpreter, const ps_ast_node *statement)
{
    assert(statement != NULL);
    assert(statement->group == PS_AST_STATEMENT);
    switch (statement->kind)
    {
    case PS_AST_ASSIGNMENT:
        return ps_ast_execute_assignment(interpreter, (const ps_ast_assignment *)statement);
    case PS_AST_IF:
        return ps_ast_execute_if(interpreter, (const ps_ast_if *)statement);
    case PS_AST_WHILE:
        return ps_ast_execute_while(interpreter, (const ps_ast_while *)statement);
    case PS_AST_REPEAT:
        return ps_ast_execute_repeat(interpreter, (const ps_ast_repeat *)statement);
    case PS_AST_FOR:
        return ps_ast_execute_for(interpreter, (const ps_ast_for *)statement);
    case PS_AST_PROCEDURE_CALL:
        return ps_ast_execute_procedure_call(interpreter, (const ps_ast_call *)statement);
    default:
        return ps_interpreter_set_message(interpreter, "Unexpected statement kind %d\n", statement->kind);
    }
}

bool ps_ast_execute_assignment(ps_interpreter *interpreter, const ps_ast_assignment *assignment)
{
    assert(assignment != NULL);
    assert(assignment->group == PS_AST_STATEMENT);
    assert(assignment->kind == PS_AST_ASSIGNMENT);
    assert(assignment->lvalue != NULL);
    assert(assignment->lvalue->group == PS_AST_GROUP_LVALUE);
    assert(assignment->lvalue->kind == PS_AST_LVALUE);
    assert(assignment->expression != NULL);
    assert(assignment->expression->group == PS_AST_EXPRESSION);
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "ASSIGNMENT:");

    ps_ast_variable *variable = assignment->lvalue;
    ps_value_type variable_type = ps_value_get_type(variable->variable->value);
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Variable: %s of type %s", variable->variable->name,
                         ps_value_type_get_name(variable_type));

    ps_ast_value value_node = {0};
    // value_node.value.type = variable->variable->value->type;
    if (!ps_ast_eval_expression(interpreter, assignment->expression, &value_node))
        return false;

    ps_value value = {.allocated = false, .type = NULL, .data = {0}};
    value.type = value_node.value.type;
    value.data = value_node.value.data;
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "{Expression value: %s}",
                         ps_value_get_display_string(&value, 0, 0));

    return ps_interpreter_set_variable_value(interpreter, variable, &value);
}

bool ps_ast_execute_if(ps_interpreter *interpreter, const ps_ast_if *if_statement)
{
    assert(if_statement != NULL);
    assert(if_statement->group == PS_AST_STATEMENT);
    assert(if_statement->kind == PS_AST_IF);
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "IF statement");

    // Evaluate condition
    ps_ast_value condition_value = {.value.allocated = false, .value.type = &ps_system_boolean, .value.data = {0}};
    if (!ps_ast_eval_expression(interpreter, if_statement->condition, &condition_value))
        return false;
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Condition value: %s",
                         ps_value_get_display_string(&condition_value.value, 0, 0));
    if (condition_value.value.type != &ps_system_boolean)
        return false;

    // Execute then or else branch
    if (condition_value.value.data.b)
    {
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Then branch: %zu statements",
                             if_statement->then_branch->count);
        return ps_ast_execute_statement_list(interpreter, if_statement->then_branch);
    }
    if (if_statement->else_branch != NULL)
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Else branch: %zu statements",
                             if_statement->else_branch->count);

    return ps_ast_execute_statement_list(interpreter, if_statement->else_branch);
}

bool ps_ast_execute_while(ps_interpreter *interpreter, const ps_ast_while *while_statement)
{
    assert(while_statement != NULL);
    assert(while_statement->group == PS_AST_STATEMENT);
    assert(while_statement->kind == PS_AST_WHILE);
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "WHILE statement: %d statements in body",
                         while_statement->body->count);

    while (true)
    {
        ps_ast_value condition_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
        bool result = ps_ast_eval_expression(interpreter, while_statement->condition, &condition_value);
        if (!result)
            return false;
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Condition value: %s",
                             ps_value_get_display_string(&condition_value.value, 0, 0));
        if (condition_value.value.type != &ps_system_boolean)
            return false;
        if (!condition_value.value.data.b)
            break;
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Body");
        if (!ps_ast_execute_statement_list(interpreter, while_statement->body))
            return false;
    }

    return true;
}

bool ps_ast_execute_repeat(ps_interpreter *interpreter, const ps_ast_repeat *repeat_statement)
{
    assert(repeat_statement != NULL);
    assert(repeat_statement->group == PS_AST_STATEMENT);
    assert(repeat_statement->kind == PS_AST_REPEAT);
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "REPEAT statement");

    ps_ast_value condition_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};

    int iteration = 0;
    do
    {
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Body %d", ++iteration);
        if (!ps_ast_execute_statement_list(interpreter, repeat_statement->body))
            return false;
        if (!ps_ast_eval_expression(interpreter, repeat_statement->condition, &condition_value))
            return false;
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Condition value: %s",
                             ps_value_get_display_string(&condition_value.value, 0, 0));
        if (condition_value.value.type != &ps_system_boolean)
            return false;
    } while (!condition_value.value.data.b);

    return true;
}

bool ps_ast_execute_for(ps_interpreter *interpreter, const ps_ast_for *for_statement)
{
    assert(for_statement != NULL);
    assert(for_statement->group == PS_AST_STATEMENT);
    assert(for_statement->kind == PS_AST_FOR);
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "FOR statement");

    ps_ast_variable *variable_simple = for_statement->variable;
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Variable: %s", variable_simple->variable->name);

    ps_ast_value start_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    if (!ps_ast_eval_expression(interpreter, for_statement->start, &start_value))
        return false;
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Start value: %s",
                         ps_value_get_display_string(&start_value.value, 0, 0));

    ps_ast_value end_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    if (!ps_ast_eval_expression(interpreter, for_statement->end, &end_value))
        return false;
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "End value: %s",
                         ps_value_get_display_string(&end_value.value, 0, 0));

    if (!ps_interpreter_set_variable_value(interpreter, variable_simple, (const ps_value *)&start_value.value))
        return false;
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Variable value: %s",
                         ps_value_get_display_string(variable_simple->variable->value, 0, 0));

    ps_value stop = {.allocated = false, .type = &ps_system_boolean, .data.b = false};
    do
    {
        // Stop if variable > finish for "TO"
        //      or variable < finish for "DOWNTO"
        ps_value iteration_value = {.allocated = false, .type = variable_simple->variable->value->type, .data = {0}};
        if (!ps_interpreter_get_variable_value(interpreter, variable_simple, &iteration_value))
            return false;
        if (!ps_operator_binary_eval(interpreter, &iteration_value, &end_value.value, &stop,
                                     for_statement->downto ? PS_OP_LT : PS_OP_GT))
            return false;
        if (stop.data.b)
        {
            ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "STOP!");
            break;
        }
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Body: %d statements", for_statement->body->count);
        if (!ps_ast_execute_statement_list(interpreter, for_statement->body))
            return false;
        bool range_check = interpreter->range_check;
        interpreter->range_check = false;
        ps_error error = for_statement->downto ? ps_function_pred(interpreter, &iteration_value, &iteration_value)
                                               : ps_function_succ(interpreter, &iteration_value, &iteration_value);
        interpreter->range_check = range_check;
        if (error != PS_ERROR_NONE)
            return false;
        if (!ps_interpreter_set_variable_value(interpreter, variable_simple, &iteration_value))
            return false;
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Variable value: %s",
                             ps_value_get_display_string(variable_simple->variable->value, 0, 0));
    } while (true);

    return true;
}

bool ps_ast_execute_procedure_write_or_writeln(ps_interpreter *interpreter, const ps_ast_call *procedure_call)
{
    assert(procedure_call != NULL);
    assert(procedure_call->group == PS_AST_STATEMENT);
    assert(procedure_call->kind == PS_AST_PROCEDURE_CALL);
    assert(procedure_call->executable == &ps_system_procedure_write ||
           procedure_call->executable == &ps_system_procedure_writeln);

    if (procedure_call->args == NULL || procedure_call->n_args == 0)
    {
        if (procedure_call->executable == &ps_system_procedure_writeln)
            fprintf(stdout, "\n");
        return true;
    }
    for (uint16_t i = 0; i < procedure_call->n_args; i += 1)
    {
        if (procedure_call->args[i] == NULL)
            continue;
        ps_ast_value arg_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
        int16_t width = procedure_call->formats[i].width;
        int16_t precision = procedure_call->formats[i].precision;
        if (!ps_ast_eval_expression(interpreter, procedure_call->args[i], &arg_value))
            return false;
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Argument %zu: %s", i,
                             ps_value_get_display_string(&arg_value.value, width, precision));
        if (procedure_call->executable == &ps_system_procedure_write &&
            !ps_procedure_write(interpreter, stdout, &arg_value.value, width, precision))
            return false;
        if (procedure_call->executable == &ps_system_procedure_writeln &&
            !ps_procedure_write(interpreter, stdout, &arg_value.value, width, precision))
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
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "System procedure: %s with %zu argument%s",
                         procedure_call->executable->name, procedure_call->n_args,
                         procedure_call->n_args > 1 ? "s" : "");
    if (procedure_call->executable == &ps_system_procedure_write ||
        procedure_call->executable == &ps_system_procedure_writeln)
    {
        // WRITE or WRITELN procedure: Evaluate and output each argument
        return ps_ast_execute_procedure_write_or_writeln(interpreter, procedure_call);
    }
    else if (procedure_call->executable == &ps_system_procedure_randomize)
    {
        // RANDOMIZE procedure (0 or 1 argument)
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "System procedure: RANDOMIZE");
        if (procedure_call->n_args == 0)
        {
            return ps_procedure_randomize(interpreter, NULL);
        }
        else if (procedure_call->n_args == 1)
        {
            ps_ast_value arg_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
            if (!ps_ast_eval_expression(interpreter, procedure_call->args[0], &arg_value))
                return false;
            ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Argument: %s",
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

bool ps_ast_execute_procedure_call(ps_interpreter *interpreter, const ps_ast_call *procedure_call)
{
    assert(procedure_call != NULL);
    assert(procedure_call->group == PS_AST_STATEMENT);
    assert(procedure_call->kind == PS_AST_PROCEDURE_CALL);
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "PROCEDURE CALL %s", procedure_call->executable->name);
    if (procedure_call->executable->system)
    {
        return ps_ast_execute_procedure_call_system(interpreter, procedure_call);
    }
    ps_ast_node *node = procedure_call->executable->value->data.n;
    if (!ps_ast_node_check_group(node, PS_AST_BLOCK))
        return ps_interpreter_set_message(interpreter, "Expected block, got %s",
                                          ps_ast_node_get_group_name(node->group));
    if (!ps_ast_node_check_kind(node, PS_AST_PROCEDURE))
        return ps_interpreter_set_message(interpreter, "Expected procedure, got %s",
                                          ps_ast_node_get_kind_name(node->kind));
    ps_ast_block *procedure = (ps_ast_block *)node;
    // Check if argument count is same as procedure declaration
    if (procedure_call->n_args != procedure->signature->parameter_count)
    {
        return ps_interpreter_set_message(interpreter, "Expected %zu arguments, got %zu",
                                          procedure->signature->parameter_count, procedure_call->n_args);
    }
    // Evaluate arguments
    ps_value parameters[procedure_call->n_args];
    ps_ast_value arg_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    for (size_t i = 0; i < procedure_call->n_args; i++)
    {
        if (!ps_ast_eval_expression(interpreter, procedure_call->args[i], &arg_value))
            return false;
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Argument: %s",
                             ps_value_get_display_string(&arg_value.value, 0, 0));
        parameters[i] = arg_value.value;
    }
    // Allocate frame for procedure
    if (!ps_interpreter_enter_frame(interpreter, procedure))
        return false;
    // Store arguments in frame
    for (size_t i = 0; i < procedure_call->n_args; i++)
    {
        ps_symbol *symbol = ps_symbol_table_find(procedure->symbols, procedure->signature->parameters[i].name);
        if (symbol == NULL)
        {
            ps_formal_parameter parameter = procedure->signature->parameters[i];
            return ps_interpreter_set_message(interpreter, "Parameter %s not found", parameter.name);
        }
        if (!ps_interpreter_copy_value(interpreter, &parameters[i], symbol->value))
            return false;
    }
    // Execute procedure with arguments on top frame of stack
    bool ok = ps_ast_execute_block(interpreter, procedure);
    ps_interpreter_exit_frame(interpreter);
    return ok;
}

bool ps_ast_execute_function_call_system(ps_interpreter *interpreter, const ps_ast_call *function_call,
                                         ps_ast_value *result)
{
    assert(function_call != NULL);
    assert(function_call->group == PS_AST_EXPRESSION);
    assert(function_call->kind == PS_AST_FUNCTION_CALL);
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "SYSTEM FUNCTION CALL %s", function_call->executable->name);
    if (function_call->executable == &ps_system_function_random)
    {
        if (function_call->n_args == 0)
        {
            if (!ps_function_random(interpreter, NULL, &result->value))
                return false;
        }
        else if (function_call->n_args == 1)
        {
            ps_ast_value ast_value = {.group = PS_AST_EXPRESSION,
                                      .kind = PS_AST_LITERAL_VALUE,
                                      .value = {.allocated = false, .type = &ps_system_none, .data = {0}}};
            if (!ps_ast_eval_expression(interpreter, function_call->args[0], &ast_value))
                return false;
            ps_error error = ps_function_random(interpreter, &ast_value.value, &result->value);
            if (error != PS_ERROR_NONE)
                return false;
            return true;
        }
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_TOO_MANY_ARGUMENTS,
                                                "RANDOM function expects 0 or 1 argument");
    }
    // all other functions have 1 argument
    if (function_call->n_args != 1)
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_TOO_MANY_ARGUMENTS,
                                                "Function %s expects 1 argument", function_call->executable->name);
    ps_ast_value ast_value = {.group = PS_AST_EXPRESSION,
                              .kind = PS_AST_LITERAL_VALUE,
                              .value = {.allocated = false, .type = &ps_system_none, .data = {0}}};
    if (!ps_ast_eval_expression(interpreter, function_call->args[0], &ast_value))
        return false;
    ps_function_1arg function = function_call->executable->value->data.x->func_1arg;
    ps_error error = function(interpreter, &ast_value.value, &result->value);
    return PS_ERROR_NONE == error;
}

bool ps_ast_execute_function_call(ps_interpreter *interpreter, const ps_ast_call *function_call, ps_ast_value *result)
{
    assert(function_call != NULL);
    assert(function_call->group == PS_AST_EXPRESSION);
    assert(function_call->kind == PS_AST_FUNCTION_CALL);
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "FUNCTION CALL %s", function_call->executable->name);
    result->value.type = &ps_system_none;
    result->value.data = (ps_value_data){0};
    if (function_call->executable->system)
    {
        return ps_ast_execute_function_call_system(interpreter, function_call, result);
    }
    return ps_interpreter_set_error_message(interpreter, PS_ERROR_NOT_IMPLEMENTED,
                                            "User Function call not implemented");
}

bool ps_ast_eval_expression(ps_interpreter *interpreter, const ps_ast_node *expression, ps_ast_value *result)
{
    assert(expression != NULL);
    assert(expression->group == PS_AST_EXPRESSION);
    assert(result != NULL);
    if (!ps_ast_node_check_group(expression, PS_AST_EXPRESSION))
        return false;
    ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "EXPRESSION @%p", (const void *)expression);
    switch (expression->kind)
    {
    case PS_AST_LITERAL_VALUE:
        const ps_ast_value *rvalue = (const ps_ast_value *)expression;
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Value: %s",
                             ps_value_get_display_string(&rvalue->value, 0, 0));
        if (!ps_interpreter_copy_value(interpreter, &rvalue->value, &result->value))
            return false;
        break;
    case PS_AST_RVALUE:
        const ps_ast_variable *variable = (const ps_ast_variable *)expression;
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Variable: %s", variable->variable->name);
        ps_value value = {.allocated = false, .type = NULL, .data = {0}};
        if (!ps_interpreter_get_variable_value(interpreter, variable, &value))
            return false;
        if (!ps_interpreter_copy_value(interpreter, &value, &result->value))
            return false;
        break;
    case PS_AST_UNARY_OPERATION:
        ps_ast_value operand_value = {.group = PS_AST_EXPRESSION,
                                      .kind = PS_AST_LITERAL_VALUE,
                                      .value.allocated = false,
                                      .value.type = &ps_system_none,
                                      .value.data = {0}};
        const ps_ast_unary_operation *unary_operation = (const ps_ast_unary_operation *)expression;
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Unary operation: %s",
                             ps_operator_unary_get_name(unary_operation->operator));
        // first evaluate operand
        if (!ps_ast_eval_expression(interpreter, unary_operation->operand, &operand_value))
            return false;
        // then apply operator to it
        if (!ps_operator_unary_eval(interpreter, &operand_value.value, &result->value, unary_operation->operator))
            return false;
        break;
    case PS_AST_BINARY_OPERATION:
        const ps_ast_binary_operation *binary_operation = (const ps_ast_binary_operation *)expression;
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Binary operation: %s",
                             ps_operator_binary_get_name(binary_operation->operator));
        // first evaluate operands, then apply operator to them
        ps_ast_value left = {.group = PS_AST_EXPRESSION,
                             .kind = PS_AST_LITERAL_VALUE,
                             .value = {.allocated = false, .type = &ps_system_none, .data = {0}}};
        if (!ps_ast_eval_expression(interpreter, binary_operation->left, &left))
            return false;
        ps_ast_value right = {.group = PS_AST_EXPRESSION,
                              .kind = PS_AST_LITERAL_VALUE,
                              .value = {.allocated = false, .type = &ps_system_none, .data = {0}}};
        if (!ps_ast_eval_expression(interpreter, binary_operation->right, &right))
            return false;
        if (!ps_operator_binary_eval(interpreter, (const ps_value *)&left.value, (const ps_value *)&right.value,
                                     &result->value, binary_operation->operator))
            return false;
        break;
    case PS_AST_FUNCTION_CALL:
        const ps_ast_call *function_call = (const ps_ast_call *)expression;
        ps_ast_debug_execute(interpreter, PS_DEBUG_VERBOSE, "Function call: %s", function_call->executable->name);
        if (!ps_ast_execute_function_call(interpreter, function_call, result))
            return false;
        break;
    default:
        ps_interpreter_set_message(interpreter, "Unexpected expression kind %s (%d)\n",
                                   ps_ast_node_get_kind_name(expression->kind), expression->kind);
        return false;
    }

    return true;
}
