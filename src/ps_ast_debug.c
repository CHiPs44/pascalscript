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
bool ps_ast_debug_prefix = false;

char *ps_ast_node_get_group_name(ps_ast_node_group group)
{
    switch (group)
    {
    case PS_AST_GROUP_UNKNOWN:
        return "UNKNOWN";
    case PS_AST_BLOCK:
        return "BLOCK";
    case PS_AST_STATEMENT:
        return "STATEMENT";
    case PS_AST_EXPRESSION:
        return "EXPRESSION";
    case PS_AST_LVALUE:
        return "LVALUE";
    default:
        ps_ast_debug_line(0, "Error: unknown AST node group %d\n", group);
        return "ERROR";
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
    default:
        ps_ast_debug_line(0, "Error: unknown AST node kind %d\n", kind);
        return "UNKNOWN";
    }
}

void ps_ast_debug_line(size_t margin, const char *format, ...) // NOSONAR
{
    if (!ps_ast_debug)
        return;
    va_list args;
    va_start(args, format);
    if (margin > 0)
        fprintf(stderr, "%*c", margin * 4, ' ');
    vfprintf(stderr, format, args); // NOSONAR
    fprintf(stderr, "\n");
    va_end(args);
}

void ps_ast_debug_word(const char *format, ...) // NOSONAR
{
    if (!ps_ast_debug)
        return;
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args); // NOSONAR
    va_end(args);
}

void ps_ast_debug_value(size_t margin, const ps_ast_value *value_node)
{
    ps_ast_debug_line(margin, "VALUE: %s", ps_value_get_debug_string(&value_node->value));
}

void ps_ast_debug_variable_simple(size_t margin, const ps_ast_variable_simple *variable_simple)
{
    ps_ast_debug_line(margin, "VARIABLE_SIMPLE: %s\n", variable_simple->variable->name);
}

void ps_ast_debug_variable_array(size_t margin, const ps_ast_variable_array *variable_array)
{
    ps_ast_debug_line(margin, "%s[", variable_array->variable->name);
    for (size_t i = 0; i < variable_array->n_indexes; i++)
    {
        ps_ast_debug_node(margin + 1, variable_array->indexes[i]);
    }
    ps_ast_debug_line(margin, "]");
}

void ps_ast_debug_statement_list(size_t margin, const ps_ast_statement_list *statement_list)
{
    ps_ast_debug_line(margin, "BEGIN {STATEMENT_LIST}");
    if (statement_list == NULL || statement_list->count == 0 || statement_list->statements == NULL)
    {
        ps_ast_debug_line(margin + 1, "{Count: %zu, Statements: NULL}",
                          statement_list == NULL ? 0 : statement_list->count);
    }
    else
    {
        ps_ast_debug_line(margin + 1, "{Count: %zu}", statement_list->count);
        for (size_t i = 0; i < statement_list->count; i++)
            ps_ast_debug_node(margin + 1, statement_list->statements[i]);
    }
    ps_ast_debug_line(margin, "END {STATEMENT_LIST}");
}

void ps_ast_debug_block(size_t margin, const ps_ast_block *block)
{
    ps_ast_debug_line(margin, "%s %s", ps_ast_node_get_kind_name(block->kind), block->name);
    // ps_ast_debug_line(margin, " - Number of symbols:     %zu", block->symbols ? block->symbols->used : 0);
    // ps_ast_debug_line(margin, " - Number of variables:   %zu", block->n_vars);
    // ps_ast_debug_line(margin, " - Number of executables: %zu", block->n_executables);
    // ps_ast_debug_line(margin, " - Number of statements:  %zu",
    //                   block->statement_list ? block->statement_list->count : 0);
    // ps_ast_debug_line(margin, " - Number of parameters:  %zu",
    //                   block->signature ? block->signature->parameter_count : 0);
    // ps_ast_debug_line(margin, " - Result type:           %s", block->result_type ? block->result_type->name :
    // "none");
    ps_ast_debug_statement_list(margin, block->statement_list);
}

void ps_ast_debug_assignment(size_t margin, const ps_ast_assignment *assignment)
{
    if (assignment->lvalue->kind == PS_AST_LVALUE_SIMPLE)
    {
        ps_ast_variable_simple *variable_simple = (ps_ast_variable_simple *)assignment->lvalue;
        ps_ast_debug_line(margin, "%s", variable_simple->variable->name);
    }
    else if (assignment->lvalue->kind == PS_AST_LVALUE_ARRAY)
    {
        ps_ast_variable_array *variable_array = (ps_ast_variable_array *)assignment->lvalue;
        ps_ast_debug_line(margin, "%s[...%zu]", variable_array->variable->name, variable_array->n_indexes);
    }
    else
    {
        ps_ast_debug_line(margin, "ASSIGNMENT with unknown lvalue kind %s (%d)\n",
                          ps_ast_node_get_kind_name(assignment->lvalue->kind), assignment->lvalue->kind);
    }
    ps_ast_debug_line(margin, ":=");
    ps_ast_debug_node(margin + 1, assignment->expression);
}

void ps_ast_debug_unary_operation(size_t margin, const ps_ast_unary_operation *unary_operation)
{
    ps_ast_debug_line(margin, "%s(", ps_operator_unary_get_name(unary_operation->operator));
    ps_ast_debug_node(margin + 1, unary_operation->operand);
    ps_ast_debug_line(margin, ")");
}

void ps_ast_debug_binary_operation(size_t margin, const ps_ast_binary_operation *binary_operation)
{
    ps_ast_debug_line(margin, "(");
    ps_ast_debug_node(margin + 1, binary_operation->left);
    ps_ast_debug_line(margin, "%s", ps_operator_binary_get_name(binary_operation->operator));
    ps_ast_debug_node(margin + 1, binary_operation->right);
    ps_ast_debug_line(margin, ")");
}

void ps_ast_debug_if(size_t margin, const ps_ast_if *if_statement)
{
    ps_ast_debug_line(margin, "IF");
    ps_ast_debug_node(margin + 1, if_statement->condition);
    ps_ast_debug_line(margin, "THEN");
    ps_ast_debug_statement_list(margin + 1, if_statement->then_branch);
    if (if_statement->then_branch != NULL)
    {
        ps_ast_debug_line(margin, "ELSE");
        ps_ast_debug_statement_list(margin + 1, if_statement->else_branch);
    }
}

void ps_ast_debug_while(size_t margin, const ps_ast_while *while_statement)
{
    ps_ast_debug_line(margin, "WHILE");
    ps_ast_debug_node(margin + 1, while_statement->condition);
    ps_ast_debug_statement_list(margin, while_statement->body);
}

void ps_ast_debug_repeat(size_t margin, const ps_ast_repeat *repeat_statement)
{
    ps_ast_debug_line(margin, "REPEAT");
    ps_ast_debug_statement_list(margin + 1, repeat_statement->body);
    ps_ast_debug_line(margin, "UNTIL");
    ps_ast_debug_node(margin + 1, repeat_statement->condition);
}

void ps_ast_debug_for(size_t margin, const ps_ast_for *for_statement)
{
    ps_ast_debug_line(margin, "FOR");
    ps_ast_debug_variable_simple(margin + 1, for_statement->variable);
    ps_ast_debug_line(margin, ":=");
    ps_ast_debug_node(margin + 1, for_statement->start);
    ps_ast_debug_line(margin, "%s", for_statement->step > 0 ? "TO" : "DOWNTO");
    ps_ast_debug_node(margin + 1, for_statement->end);
    ps_ast_debug_statement_list(margin, for_statement->body);
}

void ps_ast_debug_procedure_call(size_t margin, const ps_ast_call *call)
{
    ps_ast_debug_line(margin, "{PROCEDURE_CALL} %s(", call->executable->name);
    for (size_t i = 0; i < call->n_args; i++)
    {
        ps_ast_debug_node(margin + 1, call->args[i]);
    }
    ps_ast_debug_line(margin, ")");
}

void ps_ast_debug_function_call(size_t margin, const ps_ast_call *call)
{
    ps_ast_debug_line(margin, "{FUNCTION_CALL} %s(", call->executable->name);
    for (size_t i = 0; i < call->n_args; i++)
    {
        ps_ast_debug_line(margin, " - Argument %zu:", i);
        ps_ast_debug_node(margin + 1, call->args[i]);
    }
    ps_ast_debug_line(margin, ")");
}

void ps_ast_debug_node(size_t margin, const ps_ast_node *node)
{
    if (node == NULL)
    {
        ps_ast_debug_line(margin, "{NODE: NULL}");
        return;
    }
    switch (node->kind)
    {
    case PS_AST_KIND_UNKNOWN:
        ps_ast_debug_line(margin, "Unknown node kind");
        break;
    case PS_AST_PROGRAM:
    case PS_AST_PROCEDURE:
    case PS_AST_FUNCTION:
    case PS_AST_UNIT:
        ps_ast_debug_block(margin, (const ps_ast_block *)node);
        break;
    case PS_AST_STATEMENT_LIST:
        ps_ast_debug_statement_list(margin, (const ps_ast_statement_list *)node);
        break;
    case PS_AST_ASSIGNMENT:
        ps_ast_debug_assignment(margin, (const ps_ast_assignment *)node);
        break;
    case PS_AST_UNARY_OPERATION:
        ps_ast_debug_unary_operation(margin, (const ps_ast_unary_operation *)node);
        break;
    case PS_AST_BINARY_OPERATION:
        ps_ast_debug_binary_operation(margin, (const ps_ast_binary_operation *)node);
        break;
    case PS_AST_FUNCTION_CALL:
        ps_ast_debug_function_call(margin, (const ps_ast_call *)node);
        break;
    case PS_AST_RVALUE_CONST:
        ps_ast_debug_value(margin, (const ps_ast_value *)node);
        break;
    case PS_AST_IF:
        ps_ast_debug_if(margin, (const ps_ast_if *)node);
        break;
    case PS_AST_CASE:
        ps_ast_debug_line(margin, "CASE statement (not implemented yet)");
        break;
    case PS_AST_WHILE:
        ps_ast_debug_while(margin, (const ps_ast_while *)node);
        break;
    case PS_AST_REPEAT:
        ps_ast_debug_repeat(margin, (const ps_ast_repeat *)node);
        break;
    case PS_AST_FOR:
        ps_ast_debug_for(margin, (const ps_ast_for *)node);
        break;
    case PS_AST_PROCEDURE_CALL:
        ps_ast_debug_procedure_call(margin, (const ps_ast_call *)node);
        break;
    case PS_AST_RVALUE_SIMPLE:
    case PS_AST_LVALUE_SIMPLE:
        ps_ast_debug_variable_simple(margin, (const ps_ast_variable_simple *)node);
        break;
    case PS_AST_RVALUE_ARRAY:
    case PS_AST_LVALUE_ARRAY:
        ps_ast_debug_variable_array(margin, (const ps_ast_variable_array *)node);
        break;
    default:
        ps_ast_debug_line(margin, "Error: unknown AST node kind %d", node->kind);
    }
}
