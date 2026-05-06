/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_interpreter.h"
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

bool ps_ast_run_program(ps_interpreter *interpreter, ps_ast_node *program)
{
    if (!ps_ast_check_kind(program, PS_AST_PROGRAM))
        return false;
    ps_ast_debug_line("PROGRAM %s;", program->block->name);
    return ps_ast_run_block(interpreter, program->block);
}

bool ps_ast_run_procedure(ps_interpreter *interpreter, ps_ast_node *procedure)
{
    if (!ps_ast_check_kind(procedure, PS_AST_PROCEDURE))
        return false;
    ps_ast_debug_line("PROCEDURE %s;", procedure->block->name);
    return ps_ast_run_block(interpreter, procedure->block);
}

bool ps_ast_run_function(ps_interpreter *interpreter, ps_ast_node *function)
{
    if (!ps_ast_check_kind(function, PS_AST_FUNCTION))
        return false;
    ps_ast_debug_line("FUNCTION %s;", function->block->name);
    return ps_ast_run_block(interpreter, function->block);
}

bool ps_ast_run_block(ps_interpreter *interpreter, ps_ast_node *block)
{
    // TODO: handle variable allocation and initialization
    bool result = ps_ast_run_statement_list(interpreter, block->statement_list);
    // TODO: handle variable release
    return result;
}

bool ps_ast_run_statement_list(ps_interpreter *interpreter, ps_ast_node_statement_list *statement_list)
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
        return ps_ast_run_assignment(interpreter, statement->assignment);
    case PS_AST_IF:
        return ps_ast_run_if(interpreter, statement->if_statement);
    case PS_AST_WHILE:
        return ps_ast_run_while(interpreter, statement->while_statement);
    case PS_AST_REPEAT:
        return ps_ast_run_repeat(interpreter, statement->repeat_statement);
    case PS_AST_FOR:
        return ps_ast_run_for(interpreter, statement->for_statement);
    case PS_AST_PROCEDURE_CALL:
        return ps_ast_run_procedure_call(interpreter, statement->procedure_call);
    default:
        ps_ast_debug_line("Error: unexpected statement kind %d\n", statement->kind);
        return false;
    }
}

bool ps_ast_run_assignment(ps_interpreter *interpreter, ps_ast_node_assignment *assignment)
{
    ps_ast_debug_line("ASSIGNMENT variable: %s", assignment->lvalue->variable_simple->variable->name);
    ps_ast_node_value value = {0};
    bool result = ps_ast_run_expression(interpreter, assignment->expression, &value);
    if (!result)
        return false;
    ps_ast_debug_line(" - Expression value: %s", ps_value_to_string(&value));
    // TODO: Perform the actual assignment
    return true;
}

bool ps_ast_run_if(ps_interpreter *interpreter, ps_ast_node_if *if_statement)
{
    ps_ast_debug_line("IF statement");
    ps_ast_node_value condition_value = {0};
    bool result = ps_ast_run_expression(interpreter, if_statement->condition, &condition_value);
    if (!result)
        return false;
    ps_ast_debug_line(" - Condition value: %s", ps_value_to_string(&condition_value));
    if (condition_value.value.type != &ps_system_boolean)
        return false;
    if (condition_value.value.data.b)
    {
        ps_ast_debug_line(" - Then branch");
        return ps_ast_run_statement_list(interpreter, &if_statement->then_branch->statement_list);
    }
    else
    {
        ps_ast_debug_line(" - Else branch");
        return ps_ast_run_statement_list(interpreter, &if_statement->else_branch->statement_list);
    }
}

bool ps_ast_run_while(ps_interpreter *interpreter, ps_ast_node_while *while_statement)
{
    ps_ast_debug_line("WHILE statement");
    while (true)
    {
        ps_ast_node_value condition_value = {0};
        bool result = ps_ast_run_expression(interpreter, while_statement->condition, &condition_value);
        if (!result)
            return false;
        ps_ast_debug_line(" - Condition value: %s", ps_value_to_string(&condition_value));
        if (condition_value.value.type != &ps_system_boolean)
            return false;
        if (!condition_value.value.data.b)
            break;
        ps_ast_debug_line(" - Body");
        if (!ps_ast_run_statement_list(interpreter, &while_statement->body->statement_list))
            return false;
    }
    return true;
}

bool ps_ast_run_repeat(ps_interpreter *interpreter, ps_ast_node_repeat *repeat_statement)
{
    ps_ast_debug_line("REPEAT statement");
    ps_ast_node_value condition_value = {0};
    do
    {
        ps_ast_debug_line(" - Body");
        if (!ps_ast_run_statement_list(interpreter, &repeat_statement->body->statement_list))
            return false;
        if (!ps_ast_run_expression(interpreter, repeat_statement->condition, &condition_value))
            return false;
        ps_ast_debug_line(" - Condition value: %s", ps_value_to_string(&condition_value));
        if (condition_value.value.type != &ps_system_boolean)
            return false;
    } while (!condition_value.value.data.b);
    return true;
}

bool ps_ast_run_for(ps_interpreter *interpreter, ps_ast_node_for *for_statement)
{
    ps_ast_debug_line("FOR statement");
    ps_ast_debug_line(" - Variable: %s", for_statement->variable->variable_simple->variable->name);
    ps_ast_node_value start_value = {0};
    bool result = ps_ast_run_expression(interpreter, for_statement->start, &start_value);
    if (!result)
        return false;
    ps_ast_debug_line(" - Start value: %s", ps_value_to_string(&start_value));
    ps_ast_node_value end_value = {0};
    result = ps_ast_run_expression(interpreter, for_statement->end, &end_value);
    if (!result)
        return false;
    ps_ast_debug_line(" - End value: %s", ps_value_to_string(&end_value));
    bool loop = true;
    while (loop)
    {
        ps_ast_debug_line(" - Body");
        if (!ps_ast_run_statement_list(interpreter, &for_statement->body->statement_list))
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

bool ps_ast_run_procedure_call(ps_interpreter *interpreter, ps_ast_node_call *procedure_call)
{
    ps_ast_debug_line("PROCEDURE CALL %s", procedure_call->executable->name);
    return false; // TODO
}

bool ps_ast_run_function_call(ps_interpreter *interpreter, ps_ast_node_call *function_call, ps_ast_node_value *result)
{
    ps_ast_debug_line("FUNCTION CALL %s", function_call->executable->name);
    return false; // TODO
}

bool ps_ast_run_expression(ps_interpreter *interpreter, ps_ast_node_expression *expression, ps_ast_node_value *result)
{
    ps_ast_debug_line("EXPRESSION");
    return false; // TODO
}
