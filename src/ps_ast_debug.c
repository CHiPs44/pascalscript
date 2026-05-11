/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_interpreter.h"
#include "ps_operator.h"
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
    case PS_AST_KIND_UNKNOWN:
        return "UNKNOWN";
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
    case PS_AST_CASE:
        return "CASE";
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
    case PS_AST_RVALUE_CONST:
        return "VALUE";
    case PS_AST_RVALUE_SIMPLE:
        return "VARIABLE_SIMPLE";
    case PS_AST_RVALUE_ARRAY:
        return "VARIABLE_ARRAY";
    case PS_AST_LVALUE_SIMPLE:
        return "LVALUE_SIMPLE";
    case PS_AST_LVALUE_ARRAY:
        return "LVALUE_ARRAY";
    case PS_AST_ARG_EXPR:
        return "ARG_EXPR";
    case PS_AST_ARG_VAR_BY_VAL:
        return "ARG_VAR_BY_VAL";
    case PS_AST_ARG_VAR_BY_REF:
        return "ARG_VAR_BY_REF";
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

void ps_ast_debug_value(const ps_ast_value *value_node)
{
    ps_ast_debug_line("VALUE: %s\n", ps_value_get_debug_string(&value_node->value));
}

void ps_ast_debug_variable_simple(const ps_ast_variable_simple *variable_simple)
{
    ps_ast_debug_line("VARIABLE_SIMPLE: %s\n", variable_simple->variable->name);
}

void ps_ast_debug_variable_array(const ps_ast_variable_array *variable_array)
{
    ps_ast_debug_line("VARIABLE_ARRAY: %s\n", variable_array->variable->name);
    ps_ast_debug_line(" - Number of indexes: %zu\n", variable_array->n_indexes);
    for (size_t i = 0; i < variable_array->n_indexes; i++)
    {
        ps_ast_debug_line(" - Index %zu:\n", i);
        ps_ast_debug_node(variable_array->indexes[i]);
    }
}

void ps_ast_debug_block(const ps_ast_block *block)
{
    ps_ast_debug_line("BLOCK kind: %s, name: %s\n", ps_ast_node_get_kind_name(block->kind), block->name);
    ps_ast_debug_line(" - Number of symbols:     %zu", block->symbols ? block->symbols->used : 0);
    ps_ast_debug_line(" - Number of variables:   %zu", block->n_vars);
    ps_ast_debug_line(" - Number of executables: %zu", block->n_executables);
    ps_ast_debug_line(" - Number of statements:  %zu", block->statement_list ? block->statement_list->count : 0);
    ps_ast_debug_line(" - Number of parameters:  %zu", block->signature ? block->signature->parameter_count : 0);
    ps_ast_debug_line(" - Result type:           %s", block->result_type ? block->result_type->name : "none");
}

void ps_ast_debug_statement_list(const ps_ast_statement_list *statement_list)
{
    ps_ast_debug_line("STATEMENT_LIST");
    ps_ast_debug_line(" - Count: %zu", statement_list->count);
    ps_ast_debug_line(" - Statements:");
    for (size_t i = 0; i < statement_list->count; i++)
    {
        ps_ast_debug_node(statement_list->statements[i]);
    }
}

void ps_ast_debug_assignment(const ps_ast_assignment *assignment)
{
    if (assignment->lvalue->kind == PS_AST_LVALUE_SIMPLE)
    {
        ps_ast_variable_simple *variable_simple = (ps_ast_variable_simple *)assignment->lvalue;
        ps_ast_debug_line("ASSIGNMENT variable: %s\n", variable_simple->variable->name);
    }
    else if (assignment->lvalue->kind == PS_AST_LVALUE_ARRAY)
    {
        ps_ast_variable_array *variable_array = (ps_ast_variable_array *)assignment->lvalue;
        ps_ast_debug_line("ASSIGNMENT variable: %s\n", variable_array->variable->name);
        ps_ast_debug_line(" - Number of indexes: %zu\n", variable_array->n_indexes);
    }
    else
    {
        ps_ast_debug_line("ASSIGNMENT with unknown lvalue kind %s (%d)\n",
                          ps_ast_node_get_kind_name(assignment->lvalue->kind), assignment->lvalue->kind);
    }
    ps_ast_debug_line(" - Expression:");
    ps_ast_debug_node(assignment->expression);
}

void ps_ast_debug_unary_operation(const ps_ast_unary_operation *unary_operation)
{
    ps_ast_debug_line("UNARY_OPERATION operator: %s\n", ps_operator_unary_get_name(unary_operation->operator));
    ps_ast_debug_line(" - Operand:");
    ps_ast_debug_node(unary_operation->operand);
}

void ps_ast_debug_binary_operation(const ps_ast_binary_operation *binary_operation)
{
    ps_ast_debug_line("BINARY_OPERATION operator: %s\n", ps_operator_binary_get_name(binary_operation->operator));
    ps_ast_debug_line(" - Left operand:");
    ps_ast_debug_node(binary_operation->left);
    ps_ast_debug_line(" - Right operand:");
    ps_ast_debug_node(binary_operation->right);
}

void ps_ast_debug_if(const ps_ast_if *if_statement)
{
    ps_ast_debug_line("IF statement\n");
    ps_ast_debug_line(" - Condition:\n");
    ps_ast_debug_node(if_statement->condition);
    ps_ast_debug_line(" - Then branch:\n");
    ps_ast_debug_statement_list(if_statement->then_branch);
    ps_ast_debug_line(" - Else branch:\n");
    ps_ast_debug_statement_list(if_statement->else_branch);
}

void ps_ast_debug_while(const ps_ast_while *while_statement)
{
    ps_ast_debug_line("WHILE statement\n");
    ps_ast_debug_line(" - Condition:\n");
    ps_ast_debug_node(while_statement->condition);
    ps_ast_debug_line(" - Body:\n");
    ps_ast_debug_statement_list(while_statement->body);
}

void ps_ast_debug_repeat(const ps_ast_repeat *repeat_statement)
{
    ps_ast_debug_line("REPEAT statement\n");
    ps_ast_debug_line(" - Body:\n");
    ps_ast_debug_statement_list(repeat_statement->body);
    ps_ast_debug_line(" - Condition:\n");
    ps_ast_debug_node(repeat_statement->condition);
}

void ps_ast_debug_for(const ps_ast_for *for_statement)
{
    ps_ast_debug_line("FOR statement\n");
    ps_ast_debug_line(" - Variable:\n");
    ps_ast_debug_variable_simple(for_statement->variable);
    ps_ast_debug_line(" - Start:\n");
    ps_ast_debug_node(for_statement->start);
    ps_ast_debug_line(" - End:\n");
    ps_ast_debug_node(for_statement->end);
    ps_ast_debug_line(" - Step: %d\n", for_statement->step);
    ps_ast_debug_line(" - Body:\n");
    ps_ast_debug_statement_list(for_statement->body);
}

void ps_ast_debug_argument(const ps_ast_argument *argument)
{
    ps_ast_debug_line("ARGUMENT group %s, kind %s\n", ps_ast_node_get_group_name(argument->group),
                      ps_ast_node_get_kind_name(argument->kind));
    ps_ast_debug_line(" - Arg:");
    ps_ast_debug_node(argument->arg);
}

void ps_ast_debug_procedure_call(const ps_ast_call *call)
{
    ps_ast_debug_line("PROCEDURE_CALL: %s\n", call->executable->name);
    ps_ast_debug_line(" - Number of arguments: %zu\n", call->n_args);
    for (size_t i = 0; i < call->n_args; i++)
    {
        ps_ast_debug_line(" - Argument %zu:\n", i);
        ps_ast_debug_argument(call->args[i]);
    }
}

void ps_ast_debug_function_call(const ps_ast_call *call)
{
    ps_ast_debug_line("FUNCTION_CALL: %s\n", call->executable->name);
    ps_ast_debug_line(" - Number of arguments: %zu\n", call->n_args);
    for (size_t i = 0; i < call->n_args; i++)
    {
        ps_ast_debug_line(" - Argument %zu:\n", i);
        ps_ast_debug_argument(call->args[i]);
    }
}

void ps_ast_debug_node(const ps_ast_node *node)
{
    if (node == NULL)
    {
        ps_ast_debug_line("NODE: NULL");
        return;
    }
    ps_ast_debug_line("NODE kind: %s (%d)\n", ps_ast_node_get_kind_name(node->kind), node->kind);
    switch (node->kind)
    {
    case PS_AST_KIND_UNKNOWN:
        ps_ast_debug_line("Unknown node kind");
        break;
    case PS_AST_PROGRAM:
    case PS_AST_PROCEDURE:
    case PS_AST_FUNCTION:
    case PS_AST_UNIT:
        ps_ast_debug_block((const ps_ast_block *)node);
        break;
    case PS_AST_STATEMENT_LIST:
        ps_ast_debug_statement_list((const ps_ast_statement_list *)node);
        break;
    case PS_AST_ASSIGNMENT:
        ps_ast_debug_assignment((const ps_ast_assignment *)node);
        break;
    case PS_AST_UNARY_OPERATION:
        ps_ast_debug_unary_operation((const ps_ast_unary_operation *)node);
        break;
    case PS_AST_BINARY_OPERATION:
        ps_ast_debug_binary_operation((const ps_ast_binary_operation *)node);
        break;
    case PS_AST_FUNCTION_CALL:
        ps_ast_debug_function_call((const ps_ast_call *)node);
        break;
    case PS_AST_RVALUE_CONST:
        ps_ast_debug_value((const ps_ast_value *)node);
        break;
    case PS_AST_IF:
        ps_ast_debug_if((const ps_ast_if *)node);
        break;
    case PS_AST_CASE:
        ps_ast_debug_line("CASE statement (not implemented yet)\n");
        break;
    case PS_AST_WHILE:
        ps_ast_debug_while((const ps_ast_while *)node);
        break;
    case PS_AST_REPEAT:
        ps_ast_debug_repeat((const ps_ast_repeat *)node);
        break;
    case PS_AST_FOR:
        ps_ast_debug_for((const ps_ast_for *)node);
        break;
    case PS_AST_PROCEDURE_CALL:
        ps_ast_debug_procedure_call((const ps_ast_call *)node);
        break;
    case PS_AST_RVALUE_SIMPLE:
    case PS_AST_LVALUE_SIMPLE:
        ps_ast_debug_variable_simple((const ps_ast_variable_simple *)node);
        break;
    case PS_AST_RVALUE_ARRAY:
    case PS_AST_LVALUE_ARRAY:
        ps_ast_debug_variable_array((const ps_ast_variable_array *)node);
        break;
    case PS_AST_ARG_EXPR:
    case PS_AST_ARG_VAR_BY_VAL:
    case PS_AST_ARG_VAR_BY_REF:
        ps_ast_debug_argument((const ps_ast_argument *)node);
        break;
    default:
        ps_ast_debug_line("Error: unknown AST node kind %d\n", node->kind);
    }
}
