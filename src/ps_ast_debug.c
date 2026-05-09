/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_interpreter.h"
#include "ps_signature.h"
#include "ps_system.h"
#include "ps_value.h"

/** @brief Global flag to enable/disable AST debug output */
bool ps_ast_debug = true;

char *ps_ast_node_get_group_name(ps_ast_node_group group)
{
    switch (group)
    {
    case PS_AST_BLOCK:
        return "BLOCK";
    case PS_AST_STATEMENT:
        return "STATEMENT";
    case PS_AST_EXPRESSION:
        return "EXPRESSION";
    case PS_AST_LVALUE:
        return "LVALUE";
    default:
        ps_ast_debug_line("Error: unknown AST node group %d\n", group);
        return "UNKNOWN";
    }
}

char *ps_ast_node_get_kind_name(ps_ast_node_kind kind)
{
    switch (kind)
    {
    case PS_AST_PROGRAM:
        return "PROGRAM";
    case PS_AST_PROCEDURE:
        return "PROCEDURE";
    case PS_AST_FUNCTION:
        return "FUNCTION";
    case PS_AST_UNIT:
        return "UNIT";
    case PS_AST_STATEMENT_LIST:
        return "STATEMENT_LIST";
    case PS_AST_ASSIGNMENT:
        return "ASSIGNMENT";
    case PS_AST_IF:
        return "IF";
    case PS_AST_WHILE:
        return "WHILE";
    case PS_AST_REPEAT:
        return "REPEAT";
    case PS_AST_FOR:
        return "FOR";
    case PS_AST_PROCEDURE_CALL:
        return "PROCEDURE_CALL";
    case PS_AST_UNARY_OPERATION:
        return "UNARY_OPERATION";
    case PS_AST_BINARY_OPERATION:
        return "BINARY_OPERATION";
    case PS_AST_FUNCTION_CALL:
        return "FUNCTION_CALL";
    case PS_AST_VALUE:
        return "VALUE";
    case PS_AST_VARIABLE_SIMPLE:
        return "VARIABLE_SIMPLE";
    case PS_AST_VARIABLE_ARRAY:
        return "VARIABLE_ARRAY";
    case PS_AST_LVALUE_SIMPLE:
        return "LVALUE_SIMPLE";
    case PS_AST_LVALUE_ARRAY:
        return "LVALUE_ARRAY";
    default:
        ps_ast_debug_line("Error: unknown AST node kind %d\n", kind);
        return "UNKNOWN";
    }
}

void ps_ast_debug_line(const char *format, ...) // NOSONAR
{
    if (!ps_ast_debug)
        return;
    va_list args;
    va_start(args, format);
    fprintf(stderr, "AST_DEBUG\t");
    vfprintf(stderr, format, args); // NOSONAR
    fprintf(stderr, "\n");
    va_end(args);
}

void ps_ast_debug_program(ps_ast_node *node)
{
    ps_ast_debug_line("PROGRAM name: %s\n", node->block->name);
    ps_ast_debug_line(" - Number of variables: %zu", node->block->n_vars);
    ps_ast_debug_line(" - Number of statements: %zu", node->block->statement_list->statement_list->count);
    ps_ast_debug_line(" - Number of executables: %zu", node->block->n_executables);
}

void ps_ast_debug_procedure(ps_ast_node *node)
{
    ps_ast_debug_line("PROCEDURE name: %s\n", node->block->name);
    ps_ast_debug_line(" - Number of parameters: %zu", node->block->signature->parameter_count);
    ps_ast_debug_line(" - Number of variables: %zu", node->block->n_vars);
    ps_ast_debug_line(" - Number of statements: %zu", node->block->statement_list->statement_list->count);
}

void ps_ast_debug_function(ps_ast_node *node)
{
    ps_ast_debug_line("FUNCTION name: %s\n", node->block->name);
    ps_ast_debug_line(" - Number of parameters: %zu", node->block->signature->parameter_count);
    ps_ast_debug_line(" - Number of variables: %zu", node->block->n_vars);
    ps_ast_debug_line(" - Number of statements: %zu", node->block->statement_list->statement_list->count);
}

void ps_ast_debug_unit(ps_ast_node *node)
{
    ps_ast_debug_line("UNIT name: %s\n", node->block->name);
    ps_ast_debug_line(" - Number of variables: %zu", node->block->n_vars);
    ps_ast_debug_line(" - Number of statements: %zu", node->block->statement_list->statement_list->count);
}

void ps_ast_debug_statement_list(ps_ast_node *node)
{
    ps_ast_debug_line("STATEMENT_LIST");
    ps_ast_debug_line(" - Count: %zu", node->block->statement_list->statement_list->count);
    ps_ast_debug_line(" - Statements:");
    for (size_t i = 0; i < node->block->statement_list->statement_list->count; i++)
    {
        ps_ast_debug_node(node->block->statement_list->statement_list->statements[i]);
    }
}

void ps_ast_debug_assignment(ps_ast_node *node)
{
    ps_ast_debug_line("ASSIGNMENT variable: %s\n", node->assignment->lvalue->variable_simple->variable->name);
    ps_ast_debug_line(" - Expression:");
    ps_ast_debug_expression(node->assignment->expression);
}

void ps_ast_debug_if(ps_ast_node *node)
{
    ps_ast_debug_line("IF statement\n");
    ps_ast_debug_line(" - Condition:\n");
    ps_ast_debug_expression(node->if_statement->condition);
    ps_ast_debug_line(" - Then branch:\n");
    ps_ast_debug_statement_list(node->if_statement->then_branch);
    ps_ast_debug_line(" - Else branch:\n");
    ps_ast_debug_statement_list(node->if_statement->else_branch);
}

void ps_ast_debug_node(ps_ast_node *node)
{
    if (node == NULL)
    {
        ps_ast_debug_line("NULL node");
        return;
    }
    ps_ast_debug_line("Node kind: %s\n", ps_ast_node_get_kind_name(node->kind));
    switch (node->kind)
    {
    case PS_AST_PROGRAM:
        ps_ast_debug_program(node);
        break;
    case PS_AST_PROCEDURE:
        ps_ast_debug_procedure(node);
        break;
    case PS_AST_FUNCTION:
        ps_ast_debug_function(node);
        break;
    case PS_AST_UNIT:
        ps_ast_debug_unit(node);
        break;
    case PS_AST_STATEMENT_LIST:
        ps_ast_debug_statement_list(node);
        break;
    case PS_AST_ASSIGNMENT:
        ps_ast_debug_assignment(node);
        break;
    case PS_AST_UNARY_OPERATION:
        ps_ast_debug_unary_operation(node);
        break;
    case PS_AST_BINARY_OPERATION:
        ps_ast_debug_binary_operation(node);
        break;
    case PS_AST_FUNCTION_CALL:
        ps_ast_debug_function_call(node);
        break;
    case PS_AST_VALUE:
        ps_ast_debug_value(node);
        break;
    case PS_AST_IF:
        ps_ast_debug_if(node);
        break;
    case PS_AST_WHILE:
        ps_ast_debug_while(node);
        break;
    case PS_AST_REPEAT:
        ps_ast_debug_repeat(node);
        break;
    case PS_AST_FOR:
        ps_ast_debug_for(node);
        break;
    case PS_AST_PROCEDURE_CALL:
        ps_ast_debug_procedure_call(node);
        break;
    case PS_AST_VARIABLE_SIMPLE:
        ps_ast_debug_variable_simple(node);
        break;
    case PS_AST_VARIABLE_ARRAY:
        ps_ast_debug_variable_array(node);
        break;
    case PS_AST_LVALUE_SIMPLE:
        ps_ast_debug_lvalue_simple(node);
        break;
    case PS_AST_LVALUE_ARRAY:
        ps_ast_debug_lvalue_array(node);
        break;
    default:
        ps_ast_debug_line("Error: unknown AST node kind %d\n", node->kind);
    }
}
