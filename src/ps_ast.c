/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_memory.h"
#include "ps_signature.h"

// =============================================================================
// ps_ast_node
// =============================================================================

ps_ast_node *ps_ast_create_node(ps_ast_node_group group, ps_ast_node_kind kind, uint16_t line, uint16_t column,
                                size_t size)
{
    ps_ast_node *node = ps_memory_calloc(PS_MEMORY_AST, 1, size);
    if (node == NULL)
        return NULL;
    node->group = group;
    node->kind = kind;
    node->line = line;
    node->column = column;
    return node;
}

ps_ast_node *ps_ast_free_node(ps_ast_node *node)
{
    if (node == NULL)
        return NULL;
    switch (node->kind)
    {
    case PS_AST_KIND_UNKNOWN:
        ps_memory_free(PS_MEMORY_AST, node);
        return NULL;
    case PS_AST_PROGRAM:
    case PS_AST_PROCEDURE:
    case PS_AST_FUNCTION:
    case PS_AST_UNIT:
        return ps_ast_free_block((ps_ast_block *)node);
    case PS_AST_STATEMENT_LIST:
        return ps_ast_free_statement_list((ps_ast_statement_list *)node);
    case PS_AST_ASSIGNMENT:
        return ps_ast_free_assignment((ps_ast_assignment *)node);
    case PS_AST_IF:
        return ps_ast_free_if((ps_ast_if *)node);
    case PS_AST_CASE:
        return NULL; // *FUTURE* implement ps_ast_free_case
    case PS_AST_WHILE:
        return ps_ast_free_while((ps_ast_while *)node);
    case PS_AST_REPEAT:
        return ps_ast_free_repeat((ps_ast_repeat *)node);
    case PS_AST_FOR:
        return ps_ast_free_for((ps_ast_for *)node);
    case PS_AST_UNARY_OPERATION:
        return ps_ast_free_unary_operation((ps_ast_unary_operation *)node);
    case PS_AST_BINARY_OPERATION:
        return ps_ast_free_binary_operation((ps_ast_binary_operation *)node);
    case PS_AST_RVALUE_CONST:
        return ps_ast_free_value((ps_ast_value *)node);
    case PS_AST_PROCEDURE_CALL:
    case PS_AST_FUNCTION_CALL:
        return ps_ast_free_call((ps_ast_call *)node);
    case PS_AST_RVALUE_SIMPLE:
    case PS_AST_LVALUE_SIMPLE:
        return ps_ast_free_variable_simple((ps_ast_variable_simple *)node);
    case PS_AST_RVALUE_ARRAY:
    case PS_AST_LVALUE_ARRAY:
        return ps_ast_free_variable_array((ps_ast_variable_array *)node);
    case PS_AST_ARG_EXPR:
    case PS_AST_ARG_VAR_BY_VAL:
    case PS_AST_ARG_VAR_BY_REF:
        return ps_ast_free_argument((ps_ast_argument *)node);
    }
}

// =============================================================================
// PS_AST_BLOCK
// =============================================================================

ps_ast_block *ps_ast_create_block(uint16_t line, uint16_t column, ps_ast_node_kind kind, char *name)
{
    assert(kind == PS_AST_PROGRAM || kind == PS_AST_PROCEDURE || kind == PS_AST_FUNCTION || kind == PS_AST_UNIT);
    ps_ast_block *block = (ps_ast_block *)ps_ast_create_node(PS_AST_BLOCK, kind, line, column, sizeof(ps_ast_block));
    if (block == NULL)
        return NULL;
    snprintf(block->name, PS_IDENTIFIER_LEN, "%s", name);
    block->symbols = NULL;
    block->signature = NULL;
    block->result_type = NULL;
    block->n_vars = 0;
    block->statement_list = NULL;
    block->n_executables = 0;
    block->executables = NULL;
    return block;
}

ps_ast_node *ps_ast_free_block(ps_ast_block *block)
{
    assert(block != NULL);
    assert(block->kind == PS_AST_PROGRAM || block->kind == PS_AST_PROCEDURE || block->kind == PS_AST_FUNCTION ||
           block->kind == PS_AST_UNIT);
    if (block->signature != NULL)
        ps_formal_signature_free(block->signature);
    if (block->symbols != NULL)
        ps_symbol_table_free(block->symbols);
    if (block->statement_list != NULL)
        ps_ast_free_statement_list(block->statement_list);
    for (size_t i = 0; i < block->n_executables; i++)
        block->executables[i] = ps_ast_free_node(block->executables[i]);
    ps_memory_free(PS_MEMORY_AST, block->executables);
    ps_memory_free(PS_MEMORY_AST, block);
    return NULL;
}

// =============================================================================
// PS_AST_STATEMENT_LIST
// =============================================================================

ps_ast_statement_list *ps_ast_create_statement_list(uint16_t line, uint16_t column, size_t count)
{
    ps_ast_statement_list *statement_list = (ps_ast_statement_list *)ps_ast_create_node(
        PS_AST_STATEMENT, PS_AST_STATEMENT_LIST, line, column, sizeof(ps_ast_statement_list));
    if (statement_list == NULL)
        return NULL;
    statement_list->count = count;
    if (count > 0)
    {
        statement_list->statements = ps_memory_calloc(PS_MEMORY_AST, count, sizeof(ps_ast_node *));
        if (statement_list->statements == NULL)
        {
            ps_memory_free(PS_MEMORY_AST, statement_list);
            return NULL;
        }
    }
    return statement_list;
}

ps_ast_node *ps_ast_free_statement_list(ps_ast_statement_list *statement_list)
{
    assert(statement_list != NULL);
    assert(statement_list->kind == PS_AST_STATEMENT_LIST);
    for (size_t i = 0; i < statement_list->count; i++)
        statement_list->statements[i] = ps_ast_free_node(statement_list->statements[i]);
    ps_memory_free(PS_MEMORY_AST, statement_list->statements);
    ps_memory_free(PS_MEMORY_AST, statement_list);
    return NULL;
}

// =============================================================================
// PS_AST_ASSIGNMENT
// =============================================================================

ps_ast_assignment *ps_ast_create_assignment(uint16_t line, uint16_t column, ps_ast_node *lvalue,
                                            ps_ast_node *expression)
{
    assert(lvalue != NULL && ps_ast_check_group(lvalue, PS_AST_LVALUE));
    assert(expression != NULL && ps_ast_check_group(expression, PS_AST_EXPRESSION));
    ps_ast_assignment *assignment = (ps_ast_assignment *)ps_ast_create_node(PS_AST_STATEMENT, PS_AST_ASSIGNMENT, line,
                                                                            column, sizeof(ps_ast_assignment));
    if (assignment == NULL)
        return NULL;
    assignment->lvalue = lvalue;
    assignment->expression = expression;
    return assignment;
}

ps_ast_node *ps_ast_free_assignment(ps_ast_assignment *assignment)
{
    assert(assignment != NULL);
    assert(assignment->kind == PS_AST_ASSIGNMENT);
    assignment->lvalue = ps_ast_free_node(assignment->lvalue);
    assignment->expression = ps_ast_free_node(assignment->expression);
    ps_memory_free(PS_MEMORY_AST, assignment);
    return NULL;
}

// =============================================================================
// PS_AST_IF
// =============================================================================

ps_ast_if *ps_ast_create_if(uint16_t line, uint16_t column, ps_ast_node *condition, ps_ast_node *then_branch,
                            ps_ast_node *else_branch)
{
    assert(condition != NULL && ps_ast_check_group(condition, PS_AST_EXPRESSION));
    assert(then_branch != NULL && ps_ast_check_group(then_branch, PS_AST_STATEMENT));
    assert(else_branch == NULL || ps_ast_check_group(else_branch, PS_AST_STATEMENT));
    ps_ast_if *if_statement =
        (ps_ast_if *)ps_ast_create_node(PS_AST_STATEMENT, PS_AST_IF, line, column, sizeof(ps_ast_if));
    if (if_statement == NULL)
        return NULL;
    if_statement->condition = condition;
    if_statement->then_branch = then_branch;
    if_statement->else_branch = else_branch;
    return if_statement;
}

ps_ast_node *ps_ast_free_if(ps_ast_if *if_statement)
{
    assert(if_statement != NULL);
    assert(if_statement->kind == PS_AST_IF);
    if_statement->condition = ps_ast_free_node(if_statement->condition);
    if_statement->then_branch = ps_ast_free_node(if_statement->then_branch);
    if_statement->else_branch = ps_ast_free_node(if_statement->else_branch);
    ps_memory_free(PS_MEMORY_AST, if_statement);
    return NULL;
}

// =============================================================================
// PS_AST_WHILE
// =============================================================================

ps_ast_while *ps_ast_create_while(uint16_t line, uint16_t column, ps_ast_node *condition, ps_ast_node *body)
{
    assert(condition != NULL && ps_ast_check_group(condition, PS_AST_EXPRESSION));
    assert(body != NULL && ps_ast_check_group(body, PS_AST_STATEMENT));
    ps_ast_while *while_statement =
        (ps_ast_while *)ps_ast_create_node(PS_AST_STATEMENT, PS_AST_WHILE, line, column, sizeof(ps_ast_while));
    if (while_statement == NULL)
        return NULL;
    while_statement->condition = condition;
    while_statement->body = body;
    return while_statement;
}

ps_ast_node *ps_ast_free_while(ps_ast_while *while_statement)
{
    assert(while_statement != NULL);
    assert(while_statement->kind == PS_AST_WHILE);
    while_statement->condition = ps_ast_free_node(while_statement->condition);
    while_statement->body = ps_ast_free_node(while_statement->body);
    ps_memory_free(PS_MEMORY_AST, while_statement);
    return NULL;
}

// =============================================================================
// PS_AST_REPEAT
// =============================================================================

ps_ast_repeat *ps_ast_create_repeat(uint16_t line, uint16_t column, ps_ast_node *body, ps_ast_node *condition)
{
    assert(body != NULL && ps_ast_check_group(body, PS_AST_STATEMENT));
    assert(condition != NULL && ps_ast_check_group(condition, PS_AST_EXPRESSION));
    ps_ast_repeat *repeat_statement =
        (ps_ast_repeat *)ps_ast_create_node(PS_AST_STATEMENT, PS_AST_REPEAT, line, column, sizeof(ps_ast_repeat));
    if (repeat_statement == NULL)
        return NULL;
    repeat_statement->body = body;
    repeat_statement->condition = condition;
    return repeat_statement;
}

ps_ast_node *ps_ast_free_repeat(ps_ast_repeat *repeat_statement)
{
    assert(repeat_statement != NULL);
    assert(repeat_statement->kind == PS_AST_REPEAT);
    repeat_statement->body = ps_ast_free_node(repeat_statement->body);
    repeat_statement->condition = ps_ast_free_node(repeat_statement->condition);
    ps_memory_free(PS_MEMORY_AST, repeat_statement);
    return NULL;
}

// =============================================================================
// PS_AST_FOR
// =============================================================================

ps_ast_for *ps_ast_create_for(uint16_t line, uint16_t column, ps_ast_variable_simple *variable, ps_ast_node *start,
                              ps_ast_node *end, int step, ps_ast_node *body)
{
    assert(variable != NULL);
    assert(start != NULL && ps_ast_check_group(start, PS_AST_EXPRESSION));
    assert(end != NULL && ps_ast_check_group(end, PS_AST_EXPRESSION));
    assert(body != NULL && ps_ast_check_group(body, PS_AST_STATEMENT));
    assert(step == 1 || step == -1);
    ps_ast_for *for_statement =
        (ps_ast_for *)ps_ast_create_node(PS_AST_STATEMENT, PS_AST_FOR, line, column, sizeof(ps_ast_for));
    if (for_statement == NULL)
        return NULL;
    for_statement->variable = variable;
    for_statement->start = start;
    for_statement->end = end;
    for_statement->step = step;
    for_statement->body = body;
    return for_statement;
}

ps_ast_node *ps_ast_free_for(ps_ast_for *for_statement)
{
    assert(for_statement != NULL);
    assert(for_statement->kind == PS_AST_FOR);
    for_statement->variable = ps_ast_free_node(for_statement->variable);
    for_statement->start = ps_ast_free_node(for_statement->start);
    for_statement->end = ps_ast_free_node(for_statement->end);
    for_statement->body = ps_ast_free_node(for_statement->body);
    ps_memory_free(PS_MEMORY_AST, for_statement);
    return NULL;
}

// =============================================================================
// PS_AST_CALL
// =============================================================================

ps_ast_call *ps_ast_create_call(uint16_t line, uint16_t column, ps_ast_node_kind kind, ps_symbol *executable,
                                size_t n_args, ps_ast_argument *args[])
{
    assert(kind == PS_AST_PROCEDURE_CALL || kind == PS_AST_FUNCTION_CALL);
    assert(executable != NULL);
    assert(n_args == 0 || args != NULL);
    ps_ast_node_group group = kind == PS_AST_PROCEDURE_CALL ? PS_AST_STATEMENT : PS_AST_EXPRESSION;
    ps_ast_call *call = (ps_ast_call *)ps_ast_create_node(group, kind, line, column, sizeof(ps_ast_call));
    if (call == NULL)
        return NULL;
    call->executable = executable;
    call->n_args = n_args;
    call->args = args;
    return call;
}

ps_ast_node *ps_ast_free_call(ps_ast_call *call)
{
    assert(call != NULL);
    assert(call->kind == PS_AST_PROCEDURE_CALL || call->kind == PS_AST_FUNCTION_CALL);
    for (size_t i = 0; i < call->n_args; i++)
        ps_ast_free_argument(&call->args[i]);
    ps_memory_free(PS_MEMORY_AST, call->args);
    ps_memory_free(PS_MEMORY_AST, call);
    return NULL;
}

// =============================================================================
// PS_AST_ARGUMENT
// =============================================================================

ps_ast_argument *ps_ast_create_argument(uint16_t line, uint16_t column, ps_ast_node_kind kind, ps_ast_node *arg)
{
    assert(kind == PS_AST_ARG_EXPR || kind == PS_AST_ARG_VAR_BY_VAL || kind == PS_AST_ARG_VAR_BY_REF);
    assert(arg != NULL);
    ps_ast_argument *argument =
        (ps_ast_argument *)ps_ast_create_node(PS_AST_ARGUMENT, kind, line, column, sizeof(ps_ast_argument));
    if (argument == NULL)
        return NULL;
    argument->arg = arg;
    return argument;
}

ps_ast_node *ps_ast_free_argument(ps_ast_argument *argument)
{
    assert(argument != NULL);
    assert(argument->kind == PS_AST_ARG_EXPR || argument->kind == PS_AST_ARG_VAR_BY_VAL ||
           argument->kind == PS_AST_ARG_VAR_BY_REF);
    argument->arg = ps_ast_free_node(argument->arg);
    ps_memory_free(PS_MEMORY_AST, argument);
    return NULL;
}

// =============================================================================
// PS_AST_UNARY_OPERATION
// =============================================================================

ps_ast_unary_operation *ps_ast_create_unary_operation(uint16_t line, uint16_t column, ps_operator_unary operator,
                                                      ps_ast_node *operand)
{
    assert(operator == PS_OP_NEG || operator == PS_OP_NOT);
    assert(operand != NULL && ps_ast_check_group(operand, PS_AST_EXPRESSION));
    ps_ast_unary_operation *unary_operation =
        ps_ast_create_node(PS_AST_EXPRESSION, PS_AST_UNARY_OPERATION, line, column, sizeof(ps_ast_unary_operation));
    if (unary_operation == NULL)
        return NULL;
    unary_operation->operator = operator;
    unary_operation->operand = operand;
    return unary_operation;
}

ps_ast_node *ps_ast_free_unary_operation(ps_ast_unary_operation *unary_operation)
{
    unary_operation->operand = ps_ast_free_node(unary_operation->operand);
    ps_memory_free(PS_MEMORY_AST, unary_operation);
    return NULL;
}

// =============================================================================
// PS_AST_BINARY_OPERATION
// =============================================================================

ps_ast_binary_operation *ps_ast_create_binary_operation(uint16_t line, uint16_t column, ps_operator_binary operator,
                                                        ps_ast_node *left, ps_ast_node *right)
{
    assert(operator == PS_OP_ADD || operator == PS_OP_SUB || operator == PS_OP_OR || operator == PS_OP_XOR ||
           operator == PS_OP_MUL || operator == PS_OP_DIV || operator == PS_OP_DIV_REAL || operator == PS_OP_MOD ||
           operator == PS_OP_AND || operator == PS_OP_SHL || operator == PS_OP_SHR || operator == PS_OP_EQ ||
           operator == PS_OP_GE || operator == PS_OP_GT || operator == PS_OP_LE || operator == PS_OP_LT ||
           operator == PS_OP_NE);
    assert(left != NULL && ps_ast_check_group(left, PS_AST_EXPRESSION));
    assert(right != NULL && ps_ast_check_group(right, PS_AST_EXPRESSION));
    ps_ast_binary_operation *binary_operation = (ps_ast_binary_operation *)ps_ast_create_node(
        PS_AST_EXPRESSION, PS_AST_BINARY_OPERATION, line, column, sizeof(ps_ast_binary_operation));
    if (binary_operation == NULL)
        return NULL;
    binary_operation->operator = operator;
    binary_operation->left = left;
    binary_operation->right = right;
    return binary_operation;
}

ps_ast_node *ps_ast_free_binary_operation(ps_ast_binary_operation *binary_operation)
{
    binary_operation->left = ps_ast_free_expression(binary_operation->left);
    binary_operation->right = ps_ast_free_expression(binary_operation->right);
    ps_memory_free(PS_MEMORY_AST, binary_operation);
    return NULL;
}

// =============================================================================
// PS_AST_RVALUE_CONST
// =============================================================================

ps_ast_value *ps_ast_create_rvalue_const(uint16_t line, uint16_t column, ps_value literal)
{
    ps_ast_value *value =
        (ps_ast_value *)ps_ast_create_node(PS_AST_EXPRESSION, PS_AST_RVALUE_CONST, line, column, sizeof(ps_ast_value));
    if (value == NULL)
        return NULL;
    value->value = literal;
    return value;
}

ps_ast_node *ps_ast_free_value(ps_ast_value *value)
{
    ps_value_free(&value->value);
    ps_memory_free(PS_MEMORY_AST, value);
    return NULL;
}

// =============================================================================
// PS_AST_RVALUE_SIMPLE
// =============================================================================

ps_ast_variable_simple *ps_ast_create_variable_simple(uint16_t line, uint16_t column, ps_ast_node_kind kind,
                                                      ps_symbol *variable)
{
    assert(kind == PS_AST_RVALUE_SIMPLE || kind == PS_AST_LVALUE_SIMPLE);
    assert(variable != NULL);
    ps_ast_node_group group = kind == PS_AST_RVALUE_SIMPLE ? PS_AST_EXPRESSION : PS_AST_LVALUE;
    ps_ast_variable_simple *variable_simple =
        (ps_ast_variable_simple *)ps_ast_create_node(group, kind, line, column, sizeof(ps_ast_variable_simple));
    if (variable_simple == NULL)
        return NULL;
    variable_simple->variable = variable;
    return variable_simple;
}

ps_ast_node *ps_ast_free_variable_simple(ps_ast_variable_simple *variable_simple)
{
    assert(variable_simple != NULL);
    assert(variable_simple->kind == PS_AST_RVALUE_SIMPLE || variable_simple->kind == PS_AST_LVALUE_SIMPLE);
    ps_memory_free(PS_MEMORY_AST, variable_simple);
    return NULL;
}

// =============================================================================
// PS_AST_VARIABLE_ARRAY
// =============================================================================

ps_ast_variable_array *ps_ast_create_variable_array(uint16_t line, uint16_t column, ps_ast_node_kind kind,
                                                    ps_symbol *variable, size_t n_indexes, ps_ast_node *indexes)
{
    assert(kind == PS_AST_RVALUE_ARRAY || kind == PS_AST_LVALUE_ARRAY);
    assert(variable != NULL);
    assert(n_indexes >= 1);
    assert(indexes != NULL);
    ps_ast_node_group group = kind == PS_AST_RVALUE_ARRAY ? PS_AST_EXPRESSION : PS_AST_LVALUE;
    ps_ast_variable_array *variable_array =
        (ps_ast_variable_array *)ps_ast_create_node(group, kind, line, column, sizeof(ps_ast_variable_array));
    if (variable_array == NULL)
        return NULL;
    variable_array->variable = variable;
    variable_array->n_indexes = n_indexes;
    variable_array->indexes = indexes;
    return variable_array;
}

ps_ast_node *ps_ast_free_variable_array(ps_ast_variable_array *variable_array)
{
    assert(variable_array != NULL);
    assert(variable_array->kind == PS_AST_LVALUE_ARRAY || variable_array->kind == PS_AST_RVALUE_ARRAY);
    for (size_t i = 0; i < variable_array->n_indexes; i++)
    {
        ps_ast_free_node(&variable_array->indexes[i]);
        variable_array->indexes[i] = NULL;
    }
    ps_memory_free(PS_MEMORY_AST, variable_array->indexes);
    ps_memory_free(PS_MEMORY_AST, variable_array);
    return NULL;
}
