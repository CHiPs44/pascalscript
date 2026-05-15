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
#include "ps_memory.h"
#include "ps_operator.h"
#include "ps_procedures.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_value.h"

bool ps_ast_run_block(ps_interpreter *interpreter, const ps_ast_block *block)
{
    bool result = false;
    if (!ps_ast_node_check_group((const ps_ast_node *)block, PS_AST_BLOCK))
        return false;
    ps_ast_debug_line(0, "BLOCK kind=%s name=%s", ps_ast_node_get_kind_name(block->kind), block->name);
    // Handle variable and parameters allocation and initialization
    size_t n_values = block->n_vars + (block->signature != NULL ? block->signature->parameter_count : 0);
    ps_value *values;
    if (n_values > 0)
    {
        values = ps_memory_calloc(PS_MEMORY_VALUE, n_values, sizeof(ps_value));
        if (values == NULL)
            return ps_interpreter_return_false(interpreter, PS_ERROR_OUT_OF_MEMORY);
    }
    else
        values = NULL;
    // Enter environment
    if (!ps_interpreter_enter_environment(interpreter, block->name, block->symbols, n_values, values))
        goto cleanup;
    // Run block statements
    result = ps_ast_run_statement_list(interpreter, block->statement_list);
    // Exit environment
    if (!ps_interpreter_exit_environment(interpreter))
        goto cleanup;
cleanup:
    // Handle variable release
    ps_memory_free(PS_MEMORY_VALUE, values);
    return result;
}

bool ps_ast_run_program(ps_interpreter *interpreter, const ps_ast_block *program)
{
    if (!ps_ast_node_check_kind((const ps_ast_node *)program, PS_AST_PROGRAM))
        return false;
    ps_ast_debug_line(0, "PROGRAM %s;", program->name);
    return ps_ast_run_block(interpreter, program);
}

bool ps_ast_run_procedure(ps_interpreter *interpreter, const ps_ast_block *procedure)
{
    if (!ps_ast_node_check_kind((const ps_ast_node *)procedure, PS_AST_PROCEDURE))
        return false;
    ps_ast_debug_line(0, "PROCEDURE %s;", procedure->name);
    return ps_ast_run_block(interpreter, procedure);
}

bool ps_ast_run_function(ps_interpreter *interpreter, const ps_ast_block *function)
{
    if (!ps_ast_node_check_kind((const ps_ast_node *)function, PS_AST_FUNCTION))
        return false;
    ps_ast_debug_line(0, "FUNCTION %s;", function->name);
    return ps_ast_run_block(interpreter, function);
}

bool ps_ast_run_statement_list(ps_interpreter *interpreter, const ps_ast_statement_list *statement_list)
{
    if (statement_list == NULL)
    {
        ps_ast_debug_line(0, "STATEMENT_LIST: NULL");
        return true;
    }
    ps_ast_debug_line(0, "STATEMENT_LIST %zu:", statement_list->count);
    for (size_t i = 0; i < statement_list->count; i++)
    {
        ps_ast_debug_line(0, "STATEMENT %zu/%zu:", i + 1, statement_list->count);
        assert(statement_list->statements != NULL);
        assert(statement_list->statements[i] != NULL);
        if (!ps_ast_run_statement(interpreter, statement_list->statements[i]))
        {
            ps_ast_debug_line(0, "STATEMENT %zu failed", i + 1);
            return false;
        }
    }
    ps_ast_debug_line(0, "STATEMENT_LIST completed");
    return true;
}

bool ps_ast_run_statement(ps_interpreter *interpreter, const ps_ast_node *statement)
{
    assert(statement != NULL);
    switch (statement->kind)
    {
    case PS_AST_ASSIGNMENT:
        return ps_ast_run_assignment(interpreter, (const ps_ast_assignment *)statement);
    case PS_AST_IF:
        return ps_ast_run_if(interpreter, (const ps_ast_if *)statement);
    case PS_AST_WHILE:
        return ps_ast_run_while(interpreter, (const ps_ast_while *)statement);
    case PS_AST_REPEAT:
        return ps_ast_run_repeat(interpreter, (const ps_ast_repeat *)statement);
    case PS_AST_FOR:
        return ps_ast_run_for(interpreter, (const ps_ast_for *)statement);
    case PS_AST_PROCEDURE_CALL:
        return ps_ast_run_procedure_call(interpreter, (const ps_ast_call *)statement);
    default:
        ps_ast_debug_line(0, "Error: unexpected statement kind %d\n", statement->kind);
        return false;
    }
}

bool ps_ast_run_assignment(ps_interpreter *interpreter, const ps_ast_assignment *assignment)
{
    assert(assignment != NULL);
    assert(assignment->kind == PS_AST_ASSIGNMENT);
    ps_ast_debug_line(0, "ASSIGNMENT:");
    switch (assignment->lvalue->kind)
    {
    case PS_AST_LVALUE_SIMPLE:
        ps_ast_variable_simple *variable_simple = ((ps_ast_variable_simple *)assignment->lvalue);
        ps_ast_debug_line(0, " - Variable: %s", variable_simple->variable->name);
        break;
    case PS_AST_LVALUE_ARRAY:
        ps_ast_variable_array *variable_array = ((ps_ast_variable_array *)assignment->lvalue);
        ps_ast_debug_line(0, " - Array: %s[%d]", variable_array->variable->name, variable_array->n_indexes);
        break;
    default:
        return false;
    }
    ps_ast_value value_node = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    if (!ps_ast_eval_expression(interpreter, assignment->expression, &value_node))
        return false;
    ps_ast_debug_line(0, " - Expression value: %s", ps_value_get_display_string(&value_node.value, 0, 0));
    switch (assignment->lvalue->kind)
    {
    case PS_AST_LVALUE_SIMPLE:
        ps_ast_variable_simple *variable_simple = ((ps_ast_variable_simple *)assignment->lvalue);
        ps_error error = ps_value_copy(&value_node.value, variable_simple->variable->value, interpreter->range_check);
        if (error != PS_ERROR_NONE)
            return false;
        break;
    case PS_AST_LVALUE_ARRAY:
        ps_interpreter_set_message(interpreter, "Array assignment not implemented yet");
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        return false;
    default:
        return false;
    }
    return true;
}

bool ps_ast_run_if(ps_interpreter *interpreter, const ps_ast_if *if_statement)
{
    assert(if_statement != NULL);
    assert(if_statement->kind == PS_AST_IF);
    ps_ast_debug_line(0, "IF statement");
    ps_ast_value condition_value = {.value.allocated = false, .value.type = &ps_system_boolean, .value.data = {0}};
    if (!ps_ast_eval_expression(interpreter, if_statement->condition, &condition_value))
        return false;
    ps_ast_debug_line(0, " - Condition value: %s", ps_value_get_display_string(&condition_value.value, 0, 0));
    if (condition_value.value.type != &ps_system_boolean)
        return false;
    if (condition_value.value.data.b)
    {
        ps_ast_debug_line(0, " - Then branch: %zu statements", if_statement->then_branch->count);
        return ps_ast_run_statement_list(interpreter, if_statement->then_branch);
    }
    else
    {
        ps_ast_debug_line(0, " - Else branch: %zu statements", if_statement->else_branch->count);
        return ps_ast_run_statement_list(interpreter, if_statement->else_branch);
    }
}

bool ps_ast_run_while(ps_interpreter *interpreter, const ps_ast_while *while_statement)
{
    assert(while_statement != NULL);
    assert(while_statement->kind == PS_AST_WHILE);
    ps_ast_debug_line(0, "WHILE statement: %d statements in body", while_statement->body->count);
    while (true)
    {
        ps_ast_value condition_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
        bool result = ps_ast_eval_expression(interpreter, while_statement->condition, &condition_value);
        if (!result)
            return false;
        ps_ast_debug_line(0, " - Condition value: %s", ps_value_get_display_string(&condition_value.value, 0, 0));
        if (condition_value.value.type != &ps_system_boolean)
            return false;
        if (!condition_value.value.data.b)
            break;
        ps_ast_debug_line(0, " - Body");
        if (!ps_ast_run_statement_list(interpreter, while_statement->body))
            return false;
    }
    return true;
}

bool ps_ast_run_repeat(ps_interpreter *interpreter, const ps_ast_repeat *repeat_statement)
{
    assert(repeat_statement != NULL);
    assert(repeat_statement->kind == PS_AST_REPEAT);
    ps_ast_debug_line(0, "REPEAT statement");
    ps_ast_value condition_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    do
    {
        ps_ast_debug_line(0, " - Body");
        if (!ps_ast_run_statement_list(interpreter, repeat_statement->body))
            return false;
        if (!ps_ast_eval_expression(interpreter, repeat_statement->condition, &condition_value))
            return false;
        ps_ast_debug_line(0, " - Condition value: %s", ps_value_get_display_string(&condition_value.value, 0, 0));
        if (condition_value.value.type != &ps_system_boolean)
            return false;
    } while (!condition_value.value.data.b);
    return true;
}

bool ps_ast_run_for(ps_interpreter *interpreter, const ps_ast_for *for_statement)
{
    assert(for_statement != NULL);
    assert(for_statement->kind == PS_AST_FOR);
    ps_ast_debug_line(0, "FOR statement");

    ps_ast_variable_simple *variable_simple = for_statement->variable;
    ps_ast_debug_line(0, " - Variable: %s", variable_simple->variable->name);

    ps_ast_value start_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    if (!ps_ast_eval_expression(interpreter, for_statement->start, &start_value))
        return false;
    ps_ast_debug_line(0, " - Start value: %s", ps_value_get_display_string(&start_value.value, 0, 0));

    ps_ast_value end_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    if (!ps_ast_eval_expression(interpreter, for_statement->end, &end_value))
        return false;
    ps_ast_debug_line(0, " - End value: %s", ps_value_get_display_string(&end_value.value, 0, 0));

    if (!ps_interpreter_copy_value(interpreter, (const ps_value *)&start_value.value, variable_simple->variable->value))
        return false;
    ps_ast_debug_line(0, " - Variable value: %s", ps_value_get_display_string(variable_simple->variable->value, 0, 0));

    bool downto = for_statement->step < 0;
    ps_value stop = {.allocated = false, .type = &ps_system_boolean, .data.b = false};
    do
    {
        // Stop if variable > finish for "TO"
        //      or variable < finish for "DOWNTO"
        interpreter->debug = DEBUG_VERBOSE;
        if (!ps_operator_eval_binary(interpreter, variable_simple->variable->value, &end_value.value, &stop,
                                     downto ? PS_OP_LT : PS_OP_GT))
        {
            ps_ast_debug_line(0, "TEST!");
            return false;
        }
        if (stop.data.b)
        {
            ps_ast_debug_line(0, "STOP!");
            break;
        }
        ps_ast_debug_line(0, " - Body: %d statements", for_statement->body->count);
        if (!ps_ast_run_statement_list(interpreter, for_statement->body))
            return false;
        bool range_check = interpreter->range_check;
        interpreter->range_check = false;
        ps_error error =
            downto ? ps_function_pred(interpreter, variable_simple->variable->value, variable_simple->variable->value)
                   : ps_function_succ(interpreter, variable_simple->variable->value, variable_simple->variable->value);
        interpreter->range_check = range_check;
        if (error != PS_ERROR_NONE)
            return false;
    } while (true);

    return true;
}

bool ps_ast_run_procedure_call_system(ps_interpreter *interpreter, const ps_ast_call *procedure_call)
{
    ps_ast_debug_line(0, " - System procedure: %s with %zu argument%s", procedure_call->executable->name,
                      procedure_call->n_args, procedure_call->n_args > 1 ? "s" : "");
    if (procedure_call->executable == &ps_system_procedure_write ||
        procedure_call->executable == &ps_system_procedure_writeln)
    {
        // WRITE or WRITELN procedure: Evaluate and output each argument
        assert(procedure_call->args != NULL);
        for (size_t i = 0; i < procedure_call->n_args; i++)
        {
            assert(procedure_call->args[i] != NULL);
            ps_ast_value arg_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
            if (!ps_ast_eval_expression(interpreter, procedure_call->args[i], &arg_value))
                return false;
            ps_ast_debug_line(0, " - Argument %zu: %s", i, ps_value_get_display_string(&arg_value.value, 0, 0));
            if (procedure_call->executable == &ps_system_procedure_writeln)
                return ps_procedure_writeln(interpreter, stdout, &arg_value.value, 0, 0);
            else
                return ps_procedure_write(interpreter, stdout, &arg_value.value, 0, 0);
        }
        if (procedure_call->executable == &ps_system_procedure_writeln)
            fprintf(stdout, "\n");
        return true;
    }
    else if (procedure_call->executable == &ps_system_procedure_randomize)
    {
        // RANDOMIZE procedure (0 or 1 argument)
        ps_ast_debug_line(0, " - System procedure: RANDOMIZE");
        if (procedure_call->n_args == 0)
        {
            return ps_procedure_randomize(interpreter, NULL);
        }
        else if (procedure_call->n_args == 1)
        {
            ps_ast_value arg_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
            if (!ps_ast_eval_expression(interpreter, procedure_call->args[0], &arg_value))
                return false;
            ps_ast_debug_line(0, " - Argument: %s", ps_value_get_display_string(&arg_value.value, 0, 0));
            return ps_procedure_randomize(interpreter, &arg_value.value);
        }
        else
        {
            ps_interpreter_set_message(interpreter, "RANDOMIZE expects 0 or 1 argument, got %zu",
                                       procedure_call->n_args);
            interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
            return false;
        }
    }
    else
    {
        ps_interpreter_set_message(interpreter, "%s not implemented yet", procedure_call->executable->name);
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        return false;
    }
}

bool ps_ast_run_procedure_call(ps_interpreter *interpreter, const ps_ast_call *procedure_call)
{
    assert(procedure_call != NULL);
    assert(procedure_call->kind == PS_AST_PROCEDURE_CALL);
    ps_ast_debug_line(0, "PROCEDURE CALL %s", procedure_call->executable->name);
    if (procedure_call->executable->system)
    {
        return ps_ast_run_procedure_call_system(interpreter, procedure_call);
    }
    ps_interpreter_set_message(interpreter, "User procedure calls not implemented yet");
    interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
    return false;
}

bool ps_ast_run_function_call(ps_interpreter *interpreter, const ps_ast_call *function_call, ps_ast_value *result)
{
    assert(function_call != NULL);
    assert(function_call->kind == PS_AST_FUNCTION_CALL);
    ps_ast_debug_line(0, "FUNCTION CALL %s", function_call->executable->name);
    result->value.type = &ps_system_none;
    result->value.data = (ps_value_data){0};
    ps_interpreter_set_message(interpreter, "Function calls not implemented yet");
    interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
    return false;
}

bool ps_ast_eval_expression(ps_interpreter *interpreter, const ps_ast_node *expression, ps_ast_value *result)
{
    assert(expression != NULL);
    assert(expression->group == PS_AST_EXPRESSION);
    assert(result != NULL);
    if (!ps_ast_node_check_group(expression, PS_AST_EXPRESSION))
        return false;
    ps_ast_debug_line(0, "EXPRESSION @%p", (const void *)expression);
    switch (expression->kind)
    {
    case PS_AST_RVALUE_CONST:
        const ps_ast_value *rvalue = (const ps_ast_value *)expression;
        ps_ast_debug_line(0, " - Value: %s", ps_value_get_display_string(&rvalue->value, 0, 0));
        if (!ps_interpreter_copy_value(interpreter, &rvalue->value, &result->value))
            return false;
        break;
    case PS_AST_RVALUE_SIMPLE:
        const ps_ast_variable_simple *variable_simple = (const ps_ast_variable_simple *)expression;
        ps_ast_debug_line(0, " - Variable: %s", variable_simple->variable->name);
        if (!ps_interpreter_copy_value(interpreter, variable_simple->variable->value, &result->value))
            return false;
        break;
    case PS_AST_RVALUE_ARRAY:
        const ps_ast_variable_array *variable_array = (const ps_ast_variable_array *)expression;
        ps_ast_debug_line(0, " - Array variable: %s[%d]", variable_array->variable->name, variable_array->n_indexes);
        ps_interpreter_set_message(interpreter, "TODO! Array access not implemented yet");
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        return false;
    case PS_AST_UNARY_OPERATION:
        ps_ast_value operand = {.group = PS_AST_EXPRESSION,
                                .kind = PS_AST_RVALUE_CONST,
                                .value.allocated = false,
                                .value.type = &ps_system_none,
                                .value.data = {0}};
        const ps_ast_unary_operation *unary_operation = (const ps_ast_unary_operation *)expression;
        ps_ast_debug_line(0, " - Unary operation: %s", ps_operator_unary_get_name(unary_operation->operator));
        // first evaluate operand, then apply operator to it
        if (!ps_ast_eval_expression(interpreter, unary_operation->operand, &operand))
            return false;
        if (!ps_operator_unary_eval(interpreter, &operand.value, &result->value, unary_operation->operator))
            return false;
        break;
    case PS_AST_BINARY_OPERATION:
        const ps_ast_binary_operation *binary_operation = (const ps_ast_binary_operation *)expression;
        ps_ast_debug_line(0, " - Binary operation: %s", ps_operator_binary_get_name(binary_operation->operator));
        // first evaluate operands, then apply operator to them
        ps_ast_value left = {.group = PS_AST_EXPRESSION,
                             .kind = PS_AST_RVALUE_CONST,
                             .value = {.allocated = false, .type = &ps_system_none, .data = {0}}};
        if (!ps_ast_eval_expression(interpreter, binary_operation->left, &left))
            return false;
        ps_ast_value right = {.group = PS_AST_EXPRESSION,
                              .kind = PS_AST_RVALUE_CONST,
                              .value = {.allocated = false, .type = &ps_system_none, .data = {0}}};
        if (!ps_ast_eval_expression(interpreter, binary_operation->right, &right))
            return false;
        if (!ps_operator_eval_binary(interpreter, (const ps_value *)&left.value, (const ps_value *)&right.value,
                                     &result->value, binary_operation->operator))
            return false;
        break;
    case PS_AST_FUNCTION_CALL:
        const ps_ast_call *function_call = (const ps_ast_call *)expression;
        ps_ast_debug_line(0, " - Function call: %s", function_call->executable->name);
        ps_interpreter_set_message(interpreter, "TODO! Function calls not implemented yet");
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        return false;
    default:
        ps_interpreter_set_message(interpreter, "Unexpected expression kind %s (%d)\n",
                                   ps_ast_node_get_kind_name(expression->kind), expression->kind);
        return false;
    }

    return true;
}
