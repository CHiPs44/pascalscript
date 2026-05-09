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

// ============================================================================
// ps_ast_node: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_node(ps_ast_node_group group, ps_ast_node_kind kind, uint16_t line, uint16_t column)
{
    ps_ast_node *node = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node));
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
    case PS_AST_PROGRAM:
    case PS_AST_PROCEDURE:
    case PS_AST_FUNCTION:
    case PS_AST_UNIT:
        return ps_ast_free_block(node);
    case PS_AST_STATEMENT_LIST:
        return ps_ast_free_statement_list(node);
    case PS_AST_ASSIGNMENT:
        return ps_ast_free_assignment(node);
    case PS_AST_IF:
        return ps_ast_free_if(node);
    case PS_AST_WHILE:
        return ps_ast_free_while(node);
    case PS_AST_REPEAT:
        return ps_ast_free_repeat(node);
    case PS_AST_FOR:
        return ps_ast_free_for(node);
    case PS_AST_PROCEDURE_CALL:
        return ps_ast_free_procedure_call(node);
    case PS_AST_UNARY_OPERATION:
        return ps_ast_free_unary_operation(node);
    case PS_AST_BINARY_OPERATION:
        return ps_ast_free_binary_operation(node);
    case PS_AST_FUNCTION_CALL:
        return ps_ast_free_function_call(node);
    case PS_AST_VALUE:
        return ps_ast_free_value(node);
    case PS_AST_VARIABLE_SIMPLE:
    case PS_AST_LVALUE_SIMPLE:
        return ps_ast_free_variable_simple(node);
    case PS_AST_VARIABLE_ARRAY:
    case PS_AST_LVALUE_ARRAY:
        return ps_ast_free_variable_array(node);
    }
}

// ============================================================================
// ps_ast_block: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_block(uint16_t line, uint16_t column, ps_ast_node_kind kind, char *name)
{
    assert(kind == PS_AST_PROGRAM || kind == PS_AST_PROCEDURE || kind == PS_AST_FUNCTION || kind == PS_AST_UNIT);
    ps_ast_node *node = ps_ast_create_node(PS_AST_GROUP_BLOCK, kind, line, column);
    if (node == NULL)
        return NULL;
    node->block = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node_block));
    if (node->block == NULL)
        return ps_ast_free_node(node);
    snprintf(node->block->name, PS_IDENTIFIER_LEN, "%s", name);
    return node;
}

ps_ast_node *ps_ast_free_block(ps_ast_node *node)
{
    ps_ast_node_block *block = node->block;
    if (block->signature != NULL)
        ps_signature_free(block->signature);
    if (block->symbols != NULL)
        ps_symbol_table_free(block->symbols);
    if (block->statement_list != NULL)
        ps_ast_free_statement_list(block->statement_list);
    for (size_t i = 0; i < block->n_executables; i++)
        block->executables[i] = ps_ast_free_node(block->executables[i]);
    ps_memory_free(PS_MEMORY_AST, block->executables);
    ps_memory_free(PS_MEMORY_AST, block);
    ps_memory_free(PS_MEMORY_AST, node);
    return NULL;
}

// ============================================================================
// ps_ast_statement_list: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_statement_list(uint16_t line, uint16_t column, size_t count)
{
    ps_ast_node *node = ps_ast_create_node(PS_AST_GROUP_STATEMENT, PS_AST_STATEMENT_LIST, line, column);
    if (node == NULL)
        return NULL;
    node->statement_list = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node_statement_list));
    if (node->statement_list == NULL)
        return ps_ast_free_node(node);
    node->statement_list->count = count;
    if (count > 0)
    {
        node->statement_list->statements = ps_memory_calloc(PS_MEMORY_AST, count, sizeof(ps_ast_node *));
        if (node->statement_list->statements == NULL)
            return ps_ast_free_node(node);
    }
    return node;
}

ps_ast_node *ps_ast_free_statement_list(ps_ast_node *node)
{
    ps_ast_node_statement_list *statement_list = node->statement_list;
    for (size_t i = 0; i < statement_list->count; i++)
        statement_list->statements[i] = ps_ast_free_node(statement_list->statements[i]);
    ps_memory_free(PS_MEMORY_AST, statement_list->statements);
    ps_memory_free(PS_MEMORY_AST, statement_list);
    return NULL;
}

// ============================================================================
// ps_ast_assignment: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_assignment(uint16_t line, uint16_t column, ps_ast_node *variable, ps_ast_node *expression)
{
    ps_ast_node *node = ps_ast_create_node(PS_AST_GROUP_STATEMENT, PS_AST_ASSIGNMENT, line, column);
    if (node == NULL)
        return NULL;
    node->assignment = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node_assignment));
    if (node->assignment == NULL)
        return ps_ast_free_node(node);
    node->assignment->lvalue = variable;
    node->assignment->expression = expression;
    return node;
}

ps_ast_node *ps_ast_free_assignment(ps_ast_node *node)
{
    ps_ast_node_assignment *assignment = node->assignment;
    assignment->lvalue = ps_ast_free_node(assignment->lvalue);
    assignment->expression = ps_ast_free_node(assignment->expression);
    ps_memory_free(PS_MEMORY_AST, assignment);
    ps_memory_free(PS_MEMORY_AST, node);
    return NULL;
}

// ============================================================================
// ps_ast_if: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_if(uint16_t line, uint16_t column, ps_ast_node *condition, ps_ast_node *then_branch,
                              ps_ast_node *else_branch)
{
    ps_ast_node *node = ps_ast_create_node(PS_AST_GROUP_STATEMENT, PS_AST_IF, line, column);
    if (node == NULL)
        return NULL;
    node->if_statement = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node_if));
    if (node->if_statement == NULL)
        return ps_ast_free_node(node);
    node->if_statement->condition = condition;
    node->if_statement->then_branch = then_branch;
    node->if_statement->else_branch = else_branch;
    return node;
}

ps_ast_node *ps_ast_free_if(ps_ast_node *node)
{
    ps_ast_node_if *if_statement = node->if_statement;
    if_statement->condition = ps_ast_free_node(if_statement->condition);
    if_statement->then_branch = ps_ast_free_node(if_statement->then_branch);
    if_statement->else_branch = ps_ast_free_node(if_statement->else_branch);
    ps_memory_free(PS_MEMORY_AST, if_statement);
    ps_memory_free(PS_MEMORY_AST, node);
    return NULL;
}

// ============================================================================
// ps_ast_while: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_while(uint16_t line, uint16_t column, ps_ast_node *condition, ps_ast_node *body)
{
    ps_ast_node *node = ps_ast_create_node(PS_AST_GROUP_STATEMENT, PS_AST_WHILE, line, column);
    if (node == NULL)
        return NULL;
    node->while_statement = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node_while));
    if (node->while_statement == NULL)
        return ps_ast_free_node(node);
    node->while_statement->condition = condition;
    node->while_statement->body = body;
    return node;
}

ps_ast_node *ps_ast_free_while(ps_ast_node *node)
{
    ps_ast_node_while *while_statement = node->while_statement;
    while_statement->condition = ps_ast_free_node(while_statement->condition);
    while_statement->body = ps_ast_free_node(while_statement->body);
    ps_memory_free(PS_MEMORY_AST, while_statement);
    ps_memory_free(PS_MEMORY_AST, node);
    return NULL;
}

// ============================================================================
// ps_ast_repeat: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_repeat(uint16_t line, uint16_t column, ps_ast_node *body, ps_ast_node *condition)
{
    ps_ast_node *node = ps_ast_create_node(PS_AST_GROUP_STATEMENT, PS_AST_REPEAT, line, column);
    if (node == NULL)
        return NULL;
    node->repeat_statement = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node_repeat));
    if (node->repeat_statement == NULL)
        return ps_ast_free_node(node);
    node->repeat_statement->body = body;
    node->repeat_statement->condition = condition;
    return node;
}

ps_ast_node *ps_ast_free_repeat(ps_ast_node *node)
{
    ps_ast_node_repeat *repeat_statement = node->repeat_statement;
    repeat_statement->body = ps_ast_free_node(repeat_statement->body);
    repeat_statement->condition = ps_ast_free_node(repeat_statement->condition);
    ps_memory_free(PS_MEMORY_AST, repeat_statement);
    ps_memory_free(PS_MEMORY_AST, node);
    return NULL;
}

// ============================================================================
// ps_ast_for: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_for(uint16_t line, uint16_t column, ps_ast_node *variable, ps_ast_node *start,
                               ps_ast_node *end, int step, ps_ast_node *body)
{
    ps_ast_node *node = ps_ast_create_node(PS_AST_GROUP_STATEMENT, PS_AST_FOR, line, column);
    if (node == NULL)
        return NULL;
    node->for_statement = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node_for));
    if (node->for_statement == NULL)
        return ps_ast_free_node(node);
    node->for_statement->variable = variable;
    node->for_statement->start = start;
    node->for_statement->end = end;
    node->for_statement->step = step;
    node->for_statement->body = body;
    return node;
}

ps_ast_node *ps_ast_free_for(ps_ast_node *node)
{
    ps_ast_node_for *for_statement = node->for_statement;
    for_statement->variable = ps_ast_free_symbol_reference(for_statement->variable);
    for_statement->start = ps_ast_free_expression(for_statement->start);
    for_statement->end = ps_ast_free_expression(for_statement->end);
    for_statement->body = ps_ast_free_node(for_statement->body);
    ps_memory_free(PS_MEMORY_AST, for_statement);
    ps_memory_free(PS_MEMORY_AST, node);
    return NULL;
}

// ============================================================================
// ps_ast_procedure_call: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_procedure_call(uint16_t line, uint16_t column, ps_symbol *executable, size_t n_args,
                                          ps_ast_node_argument *args)
{
    ps_ast_node *node = ps_ast_create_node(PS_AST_GROUP_STATEMENT, PS_AST_PROCEDURE_CALL, line, column);
    if (node == NULL)
        return NULL;
    node->procedure_call = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node_call));
    if (node->procedure_call == NULL)
        return ps_ast_free_node(node);
    node->procedure_call->executable = executable;
    node->procedure_call->n_args = n_args;
    node->procedure_call->args = args;
    return node;
}

ps_ast_node *ps_ast_free_procedure_call(ps_ast_node *node)
{
    ps_ast_node_call *procedure_call = node->procedure_call;
    for (size_t i = 0; i < procedure_call->n_args; i++)
        ps_ast_free_argument(&procedure_call->args[i]);
    ps_memory_free(PS_MEMORY_AST, procedure_call->args);
    ps_memory_free(PS_MEMORY_AST, procedure_call);
    ps_memory_free(PS_MEMORY_AST, node);
    return NULL;
}

// ============================================================================
// ps_ast_unary_operation: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_unary_operation(uint16_t line, uint16_t column, ps_operator_unary operator,
                                           ps_ast_node *operand)
{
    ps_ast_node *node = ps_ast_create_node(PS_AST_GROUP_EXPRESSION, PS_AST_UNARY_OPERATION, line, column);
    if (node == NULL)
        return NULL;
    node->unary_operation = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node_unary_operation));
    if (node->unary_operation == NULL)
        return ps_ast_free_node(node);
    node->unary_operation->operator = operator;
    node->unary_operation->operand = operand;
    return node;
}

ps_ast_node *ps_ast_free_unary_operation(ps_ast_node *node)
{
    ps_ast_node_unary_operation *unary_operation = node->unary_operation;
    unary_operation->operand = ps_ast_free_expression(unary_operation->operand);
    ps_memory_free(PS_MEMORY_AST, unary_operation);
    ps_memory_free(PS_MEMORY_AST, node);
    return NULL;
}

// ============================================================================
// ps_ast_binary_operation: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_binary_operation(uint16_t line, uint16_t column, ps_operator_binary operator,
                                            ps_ast_node *left, ps_ast_node *right)
{
    ps_ast_node *node = ps_ast_create_node(PS_AST_GROUP_EXPRESSION, PS_AST_BINARY_OPERATION, line, column);
    if (node == NULL)
        return NULL;
    node->binary_operation = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node_binary_operation));
    if (node->binary_operation == NULL)
        return ps_ast_free_node(node);
    node->binary_operation->operator = operator;
    node->binary_operation->left = left;
    node->binary_operation->right = right;
    return node;
}

ps_ast_node *ps_ast_free_binary_operation(ps_ast_node *node)
{
    ps_ast_node_binary_operation *binary_operation = node->binary_operation;
    binary_operation->left = ps_ast_free_expression(binary_operation->left);
    binary_operation->right = ps_ast_free_expression(binary_operation->right);
    ps_memory_free(PS_MEMORY_AST, binary_operation);
    ps_memory_free(PS_MEMORY_AST, node);
    return NULL;
}

// ============================================================================
// ps_ast_value: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_value(uint16_t line, uint16_t column, ps_value value)
{
    ps_ast_node *node = ps_ast_create_node(PS_AST_GROUP_EXPRESSION, PS_AST_VALUE, line, column);
    if (node == NULL)
        return NULL;
    node->value = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node_value));
    if (node->value == NULL)
        return ps_ast_free_node(node);
    node->value->value = value;
    return node;
}

ps_ast_node *ps_ast_free_value(ps_ast_node *node)
{
    ps_ast_node_value *value = node->value;
    ps_memory_free(PS_MEMORY_AST, value);
    ps_memory_free(PS_MEMORY_AST, node);
    return NULL;
}

// ============================================================================
// ps_ast_variable_simple: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_variable_simple(uint16_t line, uint16_t column, ps_symbol *variable)
{
    ps_ast_node *node = ps_ast_create_node(PS_AST_GROUP_EXPRESSION, PS_AST_VARIABLE_SIMPLE, line, column);
    if (node == NULL)
        return NULL;
    node->variable_simple = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node_variable_simple));
    if (node->variable_simple == NULL)
        return ps_ast_free_node(node);
    node->variable_simple->variable = variable;
    return node;
}

ps_ast_node *ps_ast_free_variable_simple(ps_ast_node *node)
{
    ps_ast_node_variable_simple *variable_simple = node->variable_simple;
    ps_memory_free(PS_MEMORY_AST, variable_simple);
    ps_memory_free(PS_MEMORY_AST, node);
    return NULL;
}

// ============================================================================
// ps_ast_variable_array: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_variable_array(uint16_t line, uint16_t column, ps_symbol *variable, size_t n_indexes,
                                          ps_ast_node *indexes)
{
    ps_ast_node *node = ps_ast_create_node(PS_AST_GROUP_EXPRESSION, PS_AST_VARIABLE_ARRAY, line, column);
    if (node == NULL)
        return NULL;
    node->variable_array = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node_variable_array));
    if (node->variable_array == NULL)
        return ps_ast_free_node(node);
    node->variable_array->variable = variable;
    node->variable_array->n_indexes = n_indexes;
    node->variable_array->indexes = indexes;
    return node;
}

ps_ast_node *ps_ast_free_variable_array(ps_ast_node *node)
{
    ps_ast_node_variable_array *variable_array = node->variable_array;
    for (size_t i = 0; i < variable_array->n_indexes; i++)
    {
        ps_ast_free_node(&variable_array->indexes[i]);
        variable_array->indexes[i] = NULL;
    }
    ps_memory_free(PS_MEMORY_AST, variable_array->indexes);
    ps_memory_free(PS_MEMORY_AST, variable_array);
    ps_memory_free(PS_MEMORY_AST, node);
    return NULL;
}

// ============================================================================
// ps_ast_function_call: Creation and Freeing
// ============================================================================

ps_ast_node *ps_ast_create_function_call(uint16_t line, uint16_t column, ps_symbol *executable, size_t n_args,
                                         ps_ast_node_argument *args)
{
    ps_ast_node *node = ps_ast_create_node(PS_AST_GROUP_EXPRESSION, PS_AST_FUNCTION_CALL, line, column);
    if (node == NULL)
        return NULL;
    node->function_call = ps_memory_calloc(PS_MEMORY_AST, 1, sizeof(ps_ast_node_call));
    if (node->function_call == NULL)
        return ps_ast_free_node(node);
    node->function_call->executable = executable;
    node->function_call->n_args = n_args;
    node->function_call->args = args;
    return node;
}

ps_ast_node *ps_ast_free_function_call(ps_ast_node *node)
{
    ps_ast_node_call *function_call = node->function_call;
    for (size_t i = 0; i < function_call->n_args; i++)
        ps_ast_free_argument(&function_call->args[i]);
    ps_memory_free(PS_MEMORY_AST, function_call->args);
    ps_memory_free(PS_MEMORY_AST, function_call);
    ps_memory_free(PS_MEMORY_AST, node);
    return NULL;
}
