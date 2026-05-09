/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_interpreter.h"
#include "ps_memory.h"
#include "ps_operator.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_value.h"

bool ps_ast_check_group(const ps_ast_node *node, ps_ast_node_group expected_group)
{
    if (node->group != expected_group)
    {
        ps_ast_debug_line("Error: expected AST node group %s but got %s\n", ps_ast_node_get_group_name(expected_group),
                          ps_ast_node_get_group_name(node->group));
        return false;
    }
    return true;
}

bool ps_ast_check_kind(const ps_ast_node *node, ps_ast_node_kind expected_kind)
{
    if (node->kind != expected_kind)
    {
        ps_ast_debug_line("Error: expected AST node kind %s but got %s\n", ps_ast_node_get_kind_name(expected_kind),
                          ps_ast_node_get_kind_name(node->kind));
        return false;
    }
    return true;
}

bool ps_ast_run_program(ps_interpreter *interpreter, ps_ast_block *program)
{
    if (!ps_ast_check_kind(program, PS_AST_PROGRAM))
        return false;
    ps_ast_debug_line("PROGRAM %s;", program->name);
    return ps_ast_run_block(interpreter, program);
}

bool ps_ast_run_procedure(ps_interpreter *interpreter, ps_ast_block *procedure)
{
    if (!ps_ast_check_kind(procedure, PS_AST_PROCEDURE))
        return false;
    ps_ast_debug_line("PROCEDURE %s;", procedure->name);
    return ps_ast_run_block(interpreter, procedure);
}

bool ps_ast_run_function(ps_interpreter *interpreter, ps_ast_block *function)
{
    if (!ps_ast_check_kind(function, PS_AST_FUNCTION))
        return false;
    ps_ast_debug_line("FUNCTION %s;", function->name);
    return ps_ast_run_block(interpreter, function);
}

bool ps_ast_run_block(ps_interpreter *interpreter, ps_ast_block *block)
{
    bool result = false;

    // Handle variable and parameters allocation and initialization
    size_t n_values = block->n_vars + (block->signature != NULL ? block->signature->parameter_count : 0);
    ps_value *values = ps_memory_calloc(PS_MEMORY_VALUE, n_values, sizeof(ps_value));
    if (values == NULL)
        return ps_interpreter_return_false(interpreter, PS_ERROR_OUT_OF_MEMORY);

    if (!ps_interpreter_enter_environment(interpreter, block->name, block->symbols, n_values, values))
        goto cleanup;
    result = ps_ast_run_statement_list(interpreter, block->statement_list);
    if (!ps_interpreter_exit_environment(interpreter))
        goto cleanup;

cleanup:
    // Handle variable release
    ps_memory_free(PS_MEMORY_VALUE, values);
    return result;
}

bool ps_ast_run_statement_list(ps_interpreter *interpreter, ps_ast_statement_list *statement_list)
{
    ps_ast_debug_line("STATEMENT_LIST %zu:", statement_list->count);
    for (size_t i = 0; i < statement_list->count; i++)
    {
        ps_ast_debug_line("STATEMENT %zu/%zu:", i + 1, statement_list->count);
        if (!ps_ast_run_statement(interpreter, statement_list->statements[i]))
            return false;
    }
    return true;
}

bool ps_ast_run_statement(ps_interpreter *interpreter, ps_ast_node *statement)
{
    switch (statement->kind)
    {
    case PS_AST_ASSIGNMENT:
        return ps_ast_run_assignment(interpreter, (ps_ast_assignment *)statement);
    case PS_AST_IF:
        return ps_ast_run_if(interpreter, (ps_ast_if *)statement);
    case PS_AST_WHILE:
        return ps_ast_run_while(interpreter, (ps_ast_while *)statement);
    case PS_AST_REPEAT:
        return ps_ast_run_repeat(interpreter, (ps_ast_repeat *)statement);
    case PS_AST_FOR:
        return ps_ast_run_for(interpreter, (ps_ast_for *)statement);
    case PS_AST_PROCEDURE_CALL:
        return ps_ast_run_procedure_call(interpreter, (ps_ast_call *)statement);
    default:
        ps_ast_debug_line("Error: unexpected statement kind %d\n", statement->kind);
        return false;
    }
}

bool ps_ast_run_assignment(ps_interpreter *interpreter, ps_ast_assignment *assignment)
{
    ps_ast_debug_line("ASSIGNMENT:");
    ps_ast_value value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    bool result = ps_ast_eval_expression(interpreter, assignment->expression, &value);
    if (!result)
        return false;
    ps_ast_debug_line(" - Expression value: %s", ps_value_to_string(&value));
    switch (assignment->lvalue->kind)
    {
    case PS_AST_VARIABLE_SIMPLE:
        ps_ast_variable_simple *variable_simple = ((ps_ast_variable_simple *)assignment->lvalue);
        ps_ast_debug_line(" - Variable: %s", variable_simple->variable->name);
        if (!ps_copy_value(&value.value, variable_simple->variable->value, interpreter->range_check))
            return false;
    case PS_AST_VARIABLE_ARRAY:
        ps_ast_variable_array *variable_array = ((ps_ast_variable_array *)assignment->lvalue);
        ps_ast_debug_line(" - Array: %s[%d]", variable_array->variable->name, variable_array->n_indexes);
        ps_interpreter_set_message(interpreter, "Array assignment not implemented yet");
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        return false;
    default:
        return false;
    }
    return true;
}

bool ps_ast_run_if(ps_interpreter *interpreter, ps_ast_if *if_statement)
{
    ps_ast_debug_line("IF statement");
    ps_ast_value condition_value = {.value.allocated = false, .value.type = &ps_system_boolean, .value.data = {0}};
    if (!ps_ast_eval_expression(interpreter, if_statement->condition, &condition_value))
        return false;
    ps_ast_debug_line(" - Condition value: %s", ps_value_to_string(&condition_value));
    if (condition_value.value.type != &ps_system_boolean)
        return false;
    if (condition_value.value.data.b)
    {
        ps_ast_debug_line(" - Then branch: %zu statements", if_statement->then_branch->count);
        return ps_ast_run_statement_list(interpreter, if_statement->then_branch);
    }
    else
    {
        ps_ast_debug_line(" - Else branch: %zu statements", if_statement->else_branch->count);
        return ps_ast_run_statement_list(interpreter, if_statement->else_branch);
    }
}

bool ps_ast_run_while(ps_interpreter *interpreter, ps_ast_while *while_statement)
{
    ps_ast_debug_line("WHILE statement: %d statements in body", while_statement->body->count);
    while (true)
    {
        ps_ast_value condition_value = {.value.allocated = false, .value.type = NULL, .value.data = {0}};
        bool result = ps_ast_eval_expression(interpreter, while_statement->condition, &condition_value);
        if (!result)
            return false;
        ps_ast_debug_line(" - Condition value: %s", ps_value_to_string(&condition_value));
        if (condition_value.value.type != &ps_system_boolean)
            return false;
        if (!condition_value.value.data.b)
            break;
        ps_ast_debug_line(" - Body");
        if (!ps_ast_run_statement_list(interpreter, while_statement->body))
            return false;
    }
    return true;
}

bool ps_ast_run_repeat(ps_interpreter *interpreter, ps_ast_repeat *repeat_statement)
{
    ps_ast_debug_line("REPEAT statement");
    ps_ast_value condition_value = {.value.allocated = false, .value.type = NULL, .value.data = {0}};
    do
    {
        ps_ast_debug_line(" - Body");
        if (!ps_ast_run_statement_list(interpreter, &repeat_statement->body))
            return false;
        if (!ps_ast_eval_expression(interpreter, repeat_statement->condition, &condition_value))
            return false;
        ps_ast_debug_line(" - Condition value: %s", ps_value_to_string(&condition_value));
        if (condition_value.value.type != &ps_system_boolean)
            return false;
    } while (!condition_value.value.data.b);
    return true;
}

bool ps_ast_run_for(ps_interpreter *interpreter, ps_ast_for *for_statement)
{
    ps_ast_debug_line("FOR statement");
    ps_ast_variable_simple *variable_simple = (ps_ast_variable_simple *)for_statement->variable;
    ps_ast_debug_line(" - Variable: %s", variable_simple->variable->name);
    ps_ast_value start_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    bool result = ps_ast_eval_expression(interpreter, for_statement->start, &start_value);
    if (!result)
        return false;
    ps_ast_debug_line(" - Start value: %s", ps_value_to_string(&start_value));
    ps_ast_value end_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    result = ps_ast_eval_expression(interpreter, for_statement->end, &end_value);
    if (!result)
        return false;
    ps_ast_debug_line(" - End value: %s", ps_value_to_string(&end_value));
    bool loop = true;
    while (loop)
    {
        ps_ast_debug_line(" - Body: %d statements", for_statement->body->count);
        if (!ps_ast_run_statement_list(interpreter, &for_statement->body))
            return false;
        if (for_statement->step > 0)
        {
            if (start_value.value.data.i >= end_value.value.data.i)
                loop = false;
            else
                start_value.value.data.i += for_statement->step;
        }
        else
        {
            if (start_value.value.data.i <= end_value.value.data.i)
                loop = false;
            else
                start_value.value.data.i += for_statement->step;
        }
    }
    return true;
}

bool ps_ast_run_procedure_call(ps_interpreter *interpreter, ps_ast_call *procedure_call)
{
    ps_ast_debug_line("PROCEDURE CALL %s", procedure_call->executable->name);
    ps_interpreter_set_message(interpreter, "Procedure calls not implemented yet");
    interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
    return false;
}

bool ps_ast_run_function_call(ps_interpreter *interpreter, ps_ast_call *function_call, ps_ast_value *result)
{
    ps_ast_debug_line("FUNCTION CALL %s", function_call->executable->name);
    ps_interpreter_set_message(interpreter, "Function calls not implemented yet");
    interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
    return false;
}

bool ps_ast_eval_expression(ps_interpreter *interpreter, ps_ast_node *expression, ps_ast_value *result)
{
    if (!ps_ast_check_group(expression, PS_AST_EXPRESSION))
        return false;
    ps_ast_debug_line("EXPRESSION @%p", (void *)expression);
    switch (expression->kind)
    {
    case PS_AST_VALUE:
        ps_ast_value *value = (ps_ast_value *)expression;
        ps_ast_debug_line(" - Value: %s", ps_value_to_string(&value->value));
        if (!ps_value_copy(&value->value, &result->value, interpreter->range_check))
            return false;
        break;
    case PS_AST_VARIABLE_SIMPLE:
        ps_ast_variable_simple *variable_simple = (ps_ast_variable_simple *)expression;
        ps_ast_debug_line(" - Variable: %s", variable_simple->variable->name);
        if (!ps_value_copy(variable_simple->variable->value, &result->value, interpreter->range_check))
            return false;
        break;
    case PS_AST_VARIABLE_ARRAY:
        ps_ast_variable_array *variable_array = (ps_ast_variable_array *)expression;
        ps_ast_debug_line(" - Array variable: %s[%d]", variable_array->variable->name, variable_array->n_indexes);
        ps_interpreter_set_message(interpreter, "TODO! Array access not implemented yet");
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        return false;
    case PS_AST_UNARY_OPERATION:
        ps_ast_value value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
        ps_ast_unary_operation *unary_operation = (ps_ast_unary_operation *)expression;
        ps_ast_debug_line(" - Unary operation: %s", ps_operator_unary_get_name(unary_operation->operator));
        // first evaluate operand, then apply operator to it
        if (!ps_ast_eval_expression(interpreter, unary_operation->operand, &value))
            return false;
        if (!ps_operator_unary_eval(interpreter, &value, &result->value, unary_operation->operator))
            return false;
        break;
    case PS_AST_BINARY_OPERATION:
        ps_ast_binary_operation *binary_operation = (ps_ast_binary_operation *)expression;
        ps_ast_debug_line(" - Binary operation: %s", ps_operator_binary_get_name(binary_operation->operator));
        // first evaluate operands, then apply operator to them
        ps_ast_value left = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
        if (!ps_ast_eval_expression(interpreter, binary_operation->left, &left))
            return false;
        ps_ast_value right = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
        if (!ps_ast_eval_expression(interpreter, binary_operation->right, &right))
            return false;
        if (!ps_operator_eval_binary(interpreter, &left, &right, &result->value, binary_operation->operator))
            return false;
        break;
    case PS_AST_FUNCTION_CALL:
        ps_ast_call *function_call = (ps_ast_call *)expression;
        ps_ast_debug_line(" - Function call: %s", function_call->executable->name);
        ps_interpreter_set_message(interpreter, "TODO! Function calls not implemented yet");
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        return false;
        break;
    default:
        ps_interpreter_set_message(interpreter, "Unexpected expression kind %s (%d)\n",
                                   ps_ast_get_kind_name(expression->kind), expression->kind);
        return false;
    }

    return true;
}
