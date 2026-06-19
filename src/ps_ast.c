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
#include "ps_system.h"

// =============================================================================
// ps_ast_node
// =============================================================================

bool ps_ast_node_check_group(const ps_ast_node *node, ps_ast_node_group expected_group)
{
    if (node->group != expected_group)
    {
        ps_ast_debug_line(0, "Error: expected AST node group %s but got %s\n",
                          ps_ast_node_get_group_name(expected_group), ps_ast_node_get_group_name(node->group));
        return false;
    }
    return true;
}

bool ps_ast_node_check_kind(const ps_ast_node *node, ps_ast_node_kind expected_kind)
{
    if (node->kind != expected_kind)
    {
        ps_ast_debug_line(0, "Error: expected AST node kind %s but got %s\n", ps_ast_node_get_kind_name(expected_kind),
                          ps_ast_node_get_kind_name(node->kind));
        return false;
    }
    return true;
}

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
    case PS_AST_LITERAL_VALUE:
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
    }
    return NULL;
}

/**
 * @brief Extract type from an AST node
 * @param node The AST node to extract type from
 * @return The symbol representing the type, or NULL if type cannot be determined
 */
static ps_symbol *ps_ast_node_get_type(const ps_ast_node *node)
{
    if (node == NULL)
        return NULL;

    switch (node->kind)
    {
    case PS_AST_LITERAL_VALUE:
        return ((ps_ast_value *)node)->value.type;
    case PS_AST_RVALUE_SIMPLE:
    case PS_AST_LVALUE_SIMPLE:
        ps_symbol *variable = ((ps_ast_variable_simple *)node)->variable;
        if (variable != NULL && variable->value != NULL)
            return variable->value->type;
        return NULL;
    case PS_AST_LVALUE_ARRAY:
    case PS_AST_RVALUE_ARRAY:
        ps_ast_debug_line(0, "Cannot get type of array %s items yet", ((ps_ast_variable_array *)node)->variable->name);
        return NULL;
    case PS_AST_UNARY_OPERATION:
        return ((ps_ast_unary_operation *)node)->result_type;
    case PS_AST_BINARY_OPERATION:
        return ((ps_ast_binary_operation *)node)->result_type;
    case PS_AST_FUNCTION_CALL:
        ps_symbol *function = ((ps_ast_call *)node)->executable;
        if (function != NULL && function->value != NULL)
            return function->value->type;
        return NULL;
    default:
        return NULL;
    }
}

// =============================================================================
// PS_AST_BLOCK: PROGRAM, PROCEDURE, FUNCTION, UNIT
// =============================================================================

ps_ast_block *ps_ast_create_block(uint16_t line, uint16_t column, ps_ast_block *parent, ps_ast_node_kind kind,
                                  const char *name)
{
    assert(kind == PS_AST_PROGRAM || kind == PS_AST_PROCEDURE || kind == PS_AST_FUNCTION || kind == PS_AST_UNIT);
    ps_ast_block *block = (ps_ast_block *)ps_ast_create_node(PS_AST_BLOCK, kind, line, column, sizeof(ps_ast_block));
    if (block == NULL)
        return NULL;
    if (name == NULL)
        memset(block->name, 0, PS_IDENTIFIER_SIZE);
    else
        snprintf(block->name, PS_IDENTIFIER_LEN, "%s", name);
    block->parent = parent;
    block->symbols = ps_symbol_table_alloc(0, 0);
    block->signature = NULL;
    block->n_vars = 0;
    block->statement_list = NULL;
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

ps_ast_assignment *ps_ast_create_assignment(uint16_t line, uint16_t column, ps_ast_node *lvalue, ps_ast_node *rvalue)
{
    fprintf(stderr, "DEBUG\tPS_AST_ASSIGNMENT\tCreating assignment node at line %u, column %u, lvalue=%p, rvalue=%p\n",
            line, column, (void *)lvalue, (void *)rvalue);
    assert(lvalue != NULL && ps_ast_node_check_group(lvalue, PS_AST_LVALUE));
    assert(rvalue != NULL && ps_ast_node_check_group(rvalue, PS_AST_EXPRESSION));
    ps_ast_assignment *assignment = (ps_ast_assignment *)ps_ast_create_node(PS_AST_STATEMENT, PS_AST_ASSIGNMENT, line,
                                                                            column, sizeof(ps_ast_assignment));
    if (assignment == NULL)
        return NULL;
    assignment->lvalue = lvalue;
    assignment->expression = rvalue;
    fprintf(stderr,
            "DEBUG\tPS_AST_ASSIGNMENT\tCreated assignment node at %p (line=%u, column=%u, lvalue=%p, rvalue=%p)\n",
            (void *)assignment, assignment->line, assignment->column, (void *)assignment->lvalue,
            (void *)assignment->expression);
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
// PS_AST_IF: IF ... THEN ... ELSE ...
// =============================================================================

ps_ast_if *ps_ast_create_if(uint16_t line, uint16_t column, ps_ast_node *condition, ps_ast_statement_list *then_branch,
                            ps_ast_statement_list *else_branch)
{
    assert(condition != NULL && ps_ast_node_check_group(condition, PS_AST_EXPRESSION));
    assert(then_branch != NULL && ps_ast_node_check_group((ps_ast_node *)then_branch, PS_AST_STATEMENT));
    assert(else_branch == NULL || ps_ast_node_check_group((ps_ast_node *)else_branch, PS_AST_STATEMENT));
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
    if_statement->then_branch = (ps_ast_statement_list *)ps_ast_free_statement_list(if_statement->then_branch);
    if_statement->else_branch = (ps_ast_statement_list *)ps_ast_free_statement_list(if_statement->else_branch);
    ps_memory_free(PS_MEMORY_AST, if_statement);
    return NULL;
}

// =============================================================================
// PS_AST_WHILE: WHILE ... DO ...
// =============================================================================

ps_ast_while *ps_ast_create_while(uint16_t line, uint16_t column, ps_ast_node *condition, ps_ast_statement_list *body)
{
    assert(condition != NULL && ps_ast_node_check_group(condition, PS_AST_EXPRESSION));
    assert(body != NULL && ps_ast_node_check_group((ps_ast_node *)body, PS_AST_STATEMENT));
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
    while_statement->body = (ps_ast_statement_list *)ps_ast_free_statement_list(while_statement->body);
    ps_memory_free(PS_MEMORY_AST, while_statement);
    return NULL;
}

// =============================================================================
// PS_AST_REPEAT: REPEAT ... UNTIL ...
// =============================================================================

ps_ast_repeat *ps_ast_create_repeat(uint16_t line, uint16_t column, ps_ast_statement_list *body, ps_ast_node *condition)
{
    assert(body != NULL && ps_ast_node_check_group((ps_ast_node *)body, PS_AST_STATEMENT));
    assert(condition != NULL && ps_ast_node_check_group(condition, PS_AST_EXPRESSION));
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
    repeat_statement->body = (ps_ast_statement_list *)ps_ast_free_statement_list(repeat_statement->body);
    repeat_statement->condition = ps_ast_free_node(repeat_statement->condition);
    ps_memory_free(PS_MEMORY_AST, repeat_statement);
    return NULL;
}

// =============================================================================
// PS_AST_FOR: FOR ... := ... TO/DOWNTO ... DO ...
// =============================================================================

ps_ast_for *ps_ast_create_for(uint16_t line, uint16_t column, ps_ast_variable_simple *variable, ps_ast_node *start,
                              ps_ast_node *end, bool downto, ps_ast_statement_list *body)
{
    assert(variable != NULL);
    assert(ps_ast_node_check_kind((ps_ast_node *)variable, PS_AST_LVALUE_SIMPLE));
    assert(start != NULL && ps_ast_node_check_group((ps_ast_node *)start, PS_AST_EXPRESSION));
    assert(end != NULL && ps_ast_node_check_group((ps_ast_node *)end, PS_AST_EXPRESSION));
    assert(body != NULL && ps_ast_node_check_group((ps_ast_node *)body, PS_AST_STATEMENT));
    ps_ast_for *for_statement =
        (ps_ast_for *)ps_ast_create_node(PS_AST_STATEMENT, PS_AST_FOR, line, column, sizeof(ps_ast_for));
    if (for_statement == NULL)
        return NULL;
    for_statement->variable = variable;
    for_statement->start = start;
    for_statement->end = end;
    for_statement->downto = downto;
    for_statement->body = body;
    return for_statement;
}

ps_ast_node *ps_ast_free_for(ps_ast_for *for_statement)
{
    assert(for_statement != NULL);
    assert(for_statement->kind == PS_AST_FOR);
    for_statement->variable = (ps_ast_variable_simple *)ps_ast_free_variable_simple(for_statement->variable);
    for_statement->start = ps_ast_free_node(for_statement->start);
    for_statement->end = ps_ast_free_node(for_statement->end);
    for_statement->body = (ps_ast_statement_list *)ps_ast_free_statement_list(for_statement->body);
    ps_memory_free(PS_MEMORY_AST, for_statement);
    return NULL;
}

// =============================================================================
// PS_AST_CALL: PROCEDURE or FUNCTION CALL
// =============================================================================

ps_ast_call *ps_ast_create_call(uint16_t line, uint16_t column, ps_ast_node_kind kind, ps_symbol *executable,
                                uint16_t n_args, ps_ast_node *args[], int16_t widths[], int16_t precisions[])
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
    call->args = ps_memory_calloc(PS_MEMORY_AST, n_args, sizeof(ps_ast_node *));
    if (call->args == NULL)
        return (ps_ast_call *)ps_ast_free_call(call);
    memcpy(call->args, args, n_args * sizeof(ps_ast_node *));
    if (widths != NULL)
    {
        call->widths = ps_memory_calloc(PS_MEMORY_AST, n_args, sizeof(int16_t));
        if (call->widths == NULL)
            return (ps_ast_call *)ps_ast_free_call(call);
        memcpy(call->widths, widths, n_args * sizeof(int16_t));
    }
    if (precisions != NULL)
    {
        call->precisions = ps_memory_calloc(PS_MEMORY_AST, n_args, sizeof(int16_t));
        if (call->precisions == NULL)
            return (ps_ast_call *)ps_ast_free_call(call);
        memcpy(call->precisions, precisions, n_args * sizeof(int16_t));
    }
    return call;
}

ps_ast_node *ps_ast_free_call(ps_ast_call *call)
{
    assert(call != NULL);
    assert(call->kind == PS_AST_PROCEDURE_CALL || call->kind == PS_AST_FUNCTION_CALL);
    if (call->args != NULL)
    {
        for (size_t i = 0; i < call->n_args; i++)
            ps_ast_free_node(call->args[i]);
        ps_memory_free(PS_MEMORY_AST, call->args);
    }
    ps_memory_free(PS_MEMORY_AST, call);
    return NULL;
}

// =============================================================================
// PS_AST_UNARY_OPERATION: - ... or NOT ...
// =============================================================================

/**
 * Get result type of unary operation
 */
ps_symbol *ps_ast_unary_operation_get_result_type(ps_operator_binary operator, ps_ast_node *operand)
{
    ps_symbol *operand_type = ps_ast_node_get_type(operand);
    if (operand_type == NULL)
        return NULL;
    ps_value_type type = ps_value_get_type(operand_type->value);
    ps_value_type base = ps_value_get_base(operand_type->value);
    // - U => I
    if (operator = PS_OP_NEG && (type == PS_TYPE_UNSIGNED || (type == PS_TYPE_SUBRANGE && base == PS_TYPE_UNSIGNED)))
        return &ps_system_integer;
    // cf. ps_value_is_number()
    if (type == PS_TYPE_UNSIGNED || type == PS_TYPE_INTEGER || type == PS_TYPE_REAL ||
        (type == PS_TYPE_SUBRANGE && base == PS_TYPE_UNSIGNED) || (type == PS_TYPE_SUBRANGE && base == PS_TYPE_INTEGER))
        return operand_type;
    return NULL;
}

ps_ast_unary_operation *ps_ast_create_unary_operation(uint16_t line, uint16_t column, ps_operator_unary operator,
                                                      ps_ast_node *operand)
{
    assert(operator == PS_OP_NEG || operator == PS_OP_NOT);
    assert(operand != NULL && ps_ast_node_check_group(operand, PS_AST_EXPRESSION));
    ps_ast_unary_operation *unary_operation = (ps_ast_unary_operation *)ps_ast_create_node(
        PS_AST_EXPRESSION, PS_AST_UNARY_OPERATION, line, column, sizeof(ps_ast_unary_operation));
    if (unary_operation == NULL)
        return NULL;
    unary_operation->operator = operator;
    unary_operation->operand = operand;
    unary_operation->result_type = ps_ast_unary_operation_get_result_type(operator, operand);
    return unary_operation;
}

ps_ast_node *ps_ast_free_unary_operation(ps_ast_unary_operation *unary_operation)
{
    unary_operation->operand = ps_ast_free_node(unary_operation->operand);
    ps_memory_free(PS_MEMORY_AST, unary_operation);
    return NULL;
}

// =============================================================================
// PS_AST_BINARY_OPERATION: +, -, *, /, DIV, MOD, AND, OR, XOR, SHL, SHR, =, <>, <, <=, >, >=
// =============================================================================

/**
 * Get result type of binary operation
 */
ps_symbol *ps_ast_binary_operation_get_result_type(ps_operator_binary operator, ps_ast_node *left, ps_ast_node *right)
{
    // Extract operand types first
    ps_symbol *left_type = ps_ast_node_get_type(left);
    ps_symbol *right_type = ps_ast_node_get_type(right);

    if (left_type == NULL || right_type == NULL)
        return NULL;

    ps_value_type left_base = ps_value_get_base(left_type->value);
    ps_value_type right_base = ps_value_get_base(right_type->value);

    // Comparison operators always return BOOLEAN
    if (operator == PS_OP_EQ || operator == PS_OP_GE || operator == PS_OP_GT || operator == PS_OP_LE ||
        operator == PS_OP_LT || operator == PS_OP_NE)
        return &ps_system_boolean;

    // Real division always returns REAL
    if (operator == PS_OP_DIV_REAL)
        return &ps_system_real;

    // String concatenation: ADD with CHAR/STRING operands
    // CC, CS, SC, SS all produce STRING
    if (operator == PS_OP_ADD)
    {
        if ((left_base == PS_TYPE_CHAR && right_base == PS_TYPE_CHAR) ||
            (left_base == PS_TYPE_CHAR && right_base == PS_TYPE_STRING) ||
            (left_base == PS_TYPE_STRING && right_base == PS_TYPE_CHAR) ||
            (left_base == PS_TYPE_STRING && right_base == PS_TYPE_STRING))
            return &ps_system_string;
    }

    // Boolean operations: AND, OR, XOR with boolean operands
    if (operator == PS_OP_AND || operator == PS_OP_OR || operator == PS_OP_XOR)
    {
        if (left_base == PS_TYPE_BOOLEAN && right_base == PS_TYPE_BOOLEAN)
            return &ps_system_boolean;
    }

    // For arithmetic and bitwise operations, determine result type based on operand types
    // Rules from ps_operator.c behavior:
    // - REAL + any number => REAL (except DIV, MOD, SHL, SHR)
    // - UNSIGNED + UNSIGNED => UNSIGNED
    // - INTEGER + UNSIGNED => INTEGER (for most ops, except special cases)
    // - UNSIGNED + INTEGER => depends on operator

    // If either is REAL, result is REAL (except for bitwise and MOD/DIV)
    if ((operator != PS_OP_AND && operator != PS_OP_OR && operator != PS_OP_XOR && operator != PS_OP_SHL &&
         operator != PS_OP_SHR && operator != PS_OP_MOD && operator != PS_OP_DIV) &&
        (left_base == PS_TYPE_REAL || right_base == PS_TYPE_REAL))
        return &ps_system_real;

    // Both unsigned
    if (left_base == PS_TYPE_UNSIGNED && right_base == PS_TYPE_UNSIGNED)
        return &ps_system_unsigned;

    // Mixed INTEGER and UNSIGNED
    if ((left_base == PS_TYPE_UNSIGNED && right_base == PS_TYPE_INTEGER) ||
        (left_base == PS_TYPE_INTEGER && right_base == PS_TYPE_UNSIGNED))
    {
        // For OR operator with mixed I/U: result is INTEGER (?)
        if (operator == PS_OP_OR)
            return &ps_system_integer;
        // For UI combinations with certain operators, result is UNSIGNED
        if (left_base == PS_TYPE_UNSIGNED && right_base == PS_TYPE_INTEGER &&
            (operator == PS_OP_AND || operator == PS_OP_XOR || operator == PS_OP_SUB))
            return &ps_system_unsigned;
        // Default mixed I/U result is INTEGER
        return &ps_system_integer;
    }

    // Both integer (or neither is unsigned/real)
    return &ps_system_integer;
}

ps_ast_binary_operation *ps_ast_create_binary_operation(uint16_t line, uint16_t column, ps_operator_binary operator,
                                                        ps_ast_node *left, ps_ast_node *right)
{
    assert(operator == PS_OP_ADD || operator == PS_OP_SUB || operator == PS_OP_OR || operator == PS_OP_XOR ||
           operator == PS_OP_MUL || operator == PS_OP_DIV || operator == PS_OP_DIV_REAL || operator == PS_OP_MOD ||
           operator == PS_OP_AND || operator == PS_OP_SHL || operator == PS_OP_SHR || operator == PS_OP_EQ ||
           operator == PS_OP_GE || operator == PS_OP_GT || operator == PS_OP_LE || operator == PS_OP_LT ||
           operator == PS_OP_NE);
    assert(left != NULL && ps_ast_node_check_group((ps_ast_node *)left, PS_AST_EXPRESSION));
    assert(right != NULL && ps_ast_node_check_group((ps_ast_node *)right, PS_AST_EXPRESSION));
    ps_ast_binary_operation *binary_operation = (ps_ast_binary_operation *)ps_ast_create_node(
        PS_AST_EXPRESSION, PS_AST_BINARY_OPERATION, line, column, sizeof(ps_ast_binary_operation));
    if (binary_operation == NULL)
        return NULL;
    binary_operation->operator = operator;
    binary_operation->left = left;
    binary_operation->right = right;
    binary_operation->result_type = ps_ast_binary_operation_get_result_type(operator, left, right);
    return binary_operation;
}

ps_ast_node *ps_ast_free_binary_operation(ps_ast_binary_operation *binary_operation)
{
    binary_operation->left = ps_ast_free_node(binary_operation->left);
    binary_operation->right = ps_ast_free_node(binary_operation->right);
    ps_memory_free(PS_MEMORY_AST, binary_operation);
    return NULL;
}

// =============================================================================
// PS_AST_LITERAL_VALUE: integer, real, string, boolean, char
// =============================================================================

ps_ast_value *ps_ast_create_literal_value(uint16_t line, uint16_t column, ps_value literal)
{
    assert(literal.type != NULL);
    assert(literal.type == &ps_system_integer || literal.type == &ps_system_unsigned ||
           literal.type == &ps_system_char || literal.type == &ps_system_boolean || literal.type == &ps_system_real ||
           literal.type == &ps_system_string);
    ps_ast_value *literal_node =
        (ps_ast_value *)ps_ast_create_node(PS_AST_EXPRESSION, PS_AST_LITERAL_VALUE, line, column, sizeof(ps_ast_value));
    if (literal_node == NULL)
        return NULL;
    literal_node->value = literal;
    return literal_node;
}

ps_ast_node *ps_ast_free_value(ps_ast_value *value)
{
    // TODO ps_value_free(&value->value);
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
    assert(variable->kind == PS_SYMBOL_KIND_VARIABLE);
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
                                                    ps_symbol *variable, size_t n_indexes, ps_ast_node **indexes)
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
        ps_ast_free_node(variable_array->indexes[i]);
        variable_array->indexes[i] = NULL;
    }
    ps_memory_free(PS_MEMORY_AST, variable_array->indexes);
    ps_memory_free(PS_MEMORY_AST, variable_array);
    return NULL;
}
