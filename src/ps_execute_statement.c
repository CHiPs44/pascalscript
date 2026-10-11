/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>

#include "ps_array.h"
#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_execute.h"
#include "ps_execute_expression.h"
#include "ps_execute_statement.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_operator.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_type_definition.h"
#include "ps_value.h"

bool ps_ast_execute_statement_list(ps_interpreter *interpreter, const ps_ast_statement_list *statement_list)
{
    if (statement_list == NULL)
        return true; // Empty statement list is valid (no-op)

    if (!ps_ast_node_check_kind((const ps_ast_node *)statement_list, PS_AST_STATEMENT_LIST))
    {
        interpreter->error_line = statement_list->line;
        interpreter->error_column = statement_list->column;
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_UNEXPECTED_AST_NODE,
                                                "Expected STATEMENT_LIST AST node");
    }
    if (statement_list->count == 0)
        return true; // Empty statement list is valid (no-op)

    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "STATEMENT_LIST %zu:", statement_list->count);

    for (int i = 0; i < statement_list->count; i++)
    {
        PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "STATEMENT %zu/%zu:", i + 1, statement_list->count);
        assert(statement_list->statements != NULL);
        assert(statement_list->statements[i] != NULL);
        if (!ps_ast_execute_statement(interpreter, statement_list->statements[i]))
        {
            PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "STATEMENT %zu failed", i + 1);
            return false;
        }
    }

    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "STATEMENT_LIST completed");

    return true;
}

bool ps_ast_execute_statement(ps_interpreter *interpreter, const ps_ast_node *statement)
{
    if (statement == NULL || statement->group != PS_AST_STATEMENT)
    {
        interpreter->error_line = statement == NULL ? 0 : statement->line;
        interpreter->error_column = statement == NULL ? 0 : statement->column;
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_UNEXPECTED_AST_NODE,
                                                "Expected STATEMENT AST node");
    }

    switch (statement->kind)
    {
    case PS_AST_ASSIGNMENT:
        return ps_ast_execute_assignment(interpreter, (const ps_ast_assignment *)statement);
    case PS_AST_IF:
        return ps_ast_execute_if(interpreter, (const ps_ast_if *)statement);
    case PS_AST_CASE:
        interpreter->error_line = statement->line;
        interpreter->error_column = statement->column;
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_NOT_IMPLEMENTED,
                                                "CASE statement not implemented");
        // return ps_ast_execute_case(interpreter, (const ps_ast_case *)statement);
    case PS_AST_WHILE:
        return ps_ast_execute_while(interpreter, (const ps_ast_while *)statement);
    case PS_AST_REPEAT:
        return ps_ast_execute_repeat(interpreter, (const ps_ast_repeat *)statement);
    case PS_AST_FOR:
        return ps_ast_execute_for(interpreter, (const ps_ast_for *)statement);
    case PS_AST_PROCEDURE_CALL:
        return ps_ast_execute_procedure_call(interpreter, (const ps_ast_call *)statement);
    default:
        interpreter->error_line = statement->line;
        interpreter->error_column = statement->column;
        return ps_interpreter_set_message(interpreter, "Unexpected statement kind %d\n", statement->kind);
    }
}

bool ps_ast_execute_assignment(ps_interpreter *interpreter, const ps_ast_assignment *assignment)
{
    assert(interpreter != NULL);
    assert(assignment != NULL);

    ps_ast_variable *lvalue = assignment->lvalue;
    bool is_array = ps_value_is_array(lvalue->variable->value);
    ps_symbol *expected_type = is_array ? ps_array_get_item_type(lvalue->variable) : lvalue->variable->value->type;
    ps_ast_value value_node = {.column = assignment->column,
                               .line = assignment->line,
                               .value.allocated = false,
                               .value.type = expected_type,
                               .value.data = {0}};
    if (!ps_ast_evaluate_expression(interpreter, assignment->expression, &value_node))
        return false; // line & column of error set when evaluating expression
    ps_value value = {.allocated = false, .type = value_node.value.type, .data = {0}};
    if (!ps_interpreter_copy_value(interpreter, &value_node.value, &value))
        goto error;
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Expression value: %s", ps_value_get_debug_string(&value));

    if (!ps_interpreter_set_variable_value(interpreter, lvalue, &value))
        goto error;

    return true;

error:
    interpreter->error_line = assignment->line;
    interpreter->error_column = assignment->column;
    return false;
}

bool ps_ast_execute_if(ps_interpreter *interpreter, const ps_ast_if *if_statement)
{
    assert(if_statement != NULL);
    assert(if_statement->group == PS_AST_STATEMENT);
    assert(if_statement->kind == PS_AST_IF);
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "IF statement");

    // Evaluate condition
    ps_ast_value condition_value = {.value.allocated = false, .value.type = &ps_system_boolean, .value.data = {0}};
    if (!ps_ast_evaluate_expression(interpreter, if_statement->condition, &condition_value))
        return false;
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Condition value: %s",
                           ps_value_get_display_string(&condition_value.value, 0, 0));
    if (condition_value.value.type != &ps_system_boolean)
        return false;

    // Execute then or else branch
    if (condition_value.value.data.b)
    {
        PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Then branch: %zu statements",
                               if_statement->then_branch->count);
        return ps_ast_execute_statement_list(interpreter, if_statement->then_branch);
    }
    if (if_statement->else_branch != NULL)
        PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Else branch: %zu statements",
                               if_statement->else_branch->count);

    return ps_ast_execute_statement_list(interpreter, if_statement->else_branch);
}

bool ps_ast_execute_while(ps_interpreter *interpreter, const ps_ast_while *while_statement)
{
    assert(while_statement != NULL);
    assert(while_statement->group == PS_AST_STATEMENT);
    assert(while_statement->kind == PS_AST_WHILE);
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "WHILE statement: %d statements in body",
                           while_statement->body->count);

    while (true)
    {
        ps_ast_value condition_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
        bool result = ps_ast_evaluate_expression(interpreter, while_statement->condition, &condition_value);
        if (!result)
            return false;
        PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Condition value: %s",
                               ps_value_get_display_string(&condition_value.value, 0, 0));
        if (condition_value.value.type != &ps_system_boolean)
            return false;
        if (!condition_value.value.data.b)
            break;
        PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Body");
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
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "REPEAT statement");

    ps_ast_value condition_value = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};

    int iteration = 0;
    do
    {
        PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Body %d", ++iteration);
        if (!ps_ast_execute_statement_list(interpreter, repeat_statement->body))
            return false;
        if (!ps_ast_evaluate_expression(interpreter, repeat_statement->condition, &condition_value))
            return false;
        PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Condition value: %s",
                               ps_value_get_display_string(&condition_value.value, 0, 0));
        if (condition_value.value.type != &ps_system_boolean)
            return false;
    } while (!condition_value.value.data.b);

    return true;
}

static bool ps_ast_execute_for_iteration(ps_interpreter *interpreter, const ps_ast_for *for_statement,
                                         const ps_ast_variable *for_variable, ps_value *iteration_value,
                                         const ps_ast_value *end_value, bool *should_stop)
{
    if (!ps_interpreter_get_variable_value(interpreter, for_variable, iteration_value))
        return false;

    ps_value stop = {.allocated = false, .type = &ps_system_boolean, .data.b = false};
    if (!ps_operator_binary_eval(interpreter, iteration_value, &end_value->value, &stop,
                                 for_statement->downto ? PS_OP_LT : PS_OP_GT))
        return false;
    *should_stop = stop.data.b;
    if (*should_stop)
    {
        PS_AST_DEBUG_EXECUTION(
            interpreter, PS_DEBUG_VERBOSE, "FOR\tSTOP! %s %s %s", ps_value_get_display_string(iteration_value, 0, 0),
            for_statement->downto ? "<=" : ">=", ps_value_get_display_string(&end_value->value, 0, 0));
        return true;
    }

    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Body: %d statements", for_statement->body->count);
    if (!ps_ast_execute_statement_list(interpreter, for_statement->body))
        return false;

    bool range_check = interpreter->range_check;
    interpreter->range_check = false;
    ps_error error = for_statement->downto ? ps_function_pred(interpreter, iteration_value, iteration_value)
                                           : ps_function_succ(interpreter, iteration_value, iteration_value);
    interpreter->range_check = range_check;
    if (error != PS_ERROR_NONE)
        return false;
    if (!ps_interpreter_set_variable_value(interpreter, for_variable, iteration_value))
        return false;
    if (!ps_interpreter_get_variable_value(interpreter, for_variable, iteration_value))
        return false;
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "FOR\tVariable value: %s",
                           ps_value_get_display_string(iteration_value, 0, 0));
    return true;
}

bool ps_ast_execute_for(ps_interpreter *interpreter, const ps_ast_for *for_statement)
{
    assert(for_statement != NULL);
    assert(for_statement->group == PS_AST_STATEMENT);
    assert(for_statement->kind == PS_AST_FOR);

    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "FOR statement");

    // Initialize variables
    // clang-format off
    ps_ast_variable *for_variable    = for_statement->variable;
    ps_ast_value     start_value     = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    ps_ast_value     end_value       = {.value.allocated = false, .value.type = &ps_system_none, .value.data = {0}};
    ps_value         iteration_value = {.allocated = false, .type = for_variable->variable->value->type, .data = {0}};
    bool             should_stop     = false;
    // clang-format on

    // Evaluate start value
    if (!ps_ast_evaluate_expression(interpreter, for_statement->start, &start_value))
        return false;
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "FOR\tStart value: %s",
                           ps_value_get_display_string(&start_value.value, 0, 0));

    // Evaluate end value
    if (!ps_ast_evaluate_expression(interpreter, for_statement->end, &end_value))
        return false;
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "FOR\tEnd value: %s",
                           ps_value_get_display_string(&end_value.value, 0, 0));

    // Set variable to start value
    if (!ps_interpreter_set_variable_value(interpreter, for_variable, (const ps_value *)&start_value.value))
        return false;
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "FOR\tVariable value: %s",
                           ps_value_get_display_string(for_variable->variable->value, 0, 0));

    do
    {
        if (!ps_ast_execute_for_iteration(interpreter, for_statement, for_variable, &iteration_value, &end_value,
                                          &should_stop))
            return false;
        if (should_stop)
            break;
    } while (true);

    return true;
}
