/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <stdio.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_executable.h"
#include "ps_interpreter.h"
#include "ps_operator.h"
#include "ps_signature.h"
#include "ps_system.h"
#include "ps_type_definition.h"
#include "ps_value.h"

/** @brief Global flag to enable/disable AST debug output */
bool ps_ast_debug = false;
bool ps_ast_debug_prefix = false;

static const char *ps_ast_node_group_names[] = {[PS_AST_GROUP_UNKNOWN] = "UNKNOWN",
                                                [PS_AST_BLOCK] = "BLOCK",
                                                [PS_AST_STATEMENT] = "STATEMENT",
                                                [PS_AST_EXPRESSION] = "EXPRESSION",
                                                [PS_AST_GROUP_LVALUE] = "LVALUE"};

const char *ps_ast_node_get_group_name(ps_ast_node_group group)
{
    static char error[16];
    unsigned int group_value = (unsigned int)group;
    if (group_value > PS_AST_GROUP_LVALUE)
    {
        snprintf(error, sizeof(error), "ERROR(%d)", group_value);
        fprintf(stderr, "ERROR: Invalid AST node group %d\n", group_value);
        return error;
    }
    return ps_ast_node_group_names[group];
}

static const char *ps_ast_node_kind_names[] = {[PS_AST_KIND_UNKNOWN] = "UNKNOWN",
                                               [PS_AST_PROGRAM] = "PROGRAM",
                                               [PS_AST_PROCEDURE] = "PROCEDURE",
                                               [PS_AST_FUNCTION] = "FUNCTION",
                                               [PS_AST_UNIT] = "UNIT",
                                               [PS_AST_STATEMENT_LIST] = "STATEMENT_LIST",
                                               [PS_AST_ASSIGNMENT] = "ASSIGNMENT",
                                               [PS_AST_IF] = "IF",
                                               [PS_AST_CASE] = "CASE",
                                               [PS_AST_WHILE] = "WHILE",
                                               [PS_AST_REPEAT] = "REPEAT",
                                               [PS_AST_FOR] = "FOR",
                                               [PS_AST_PROCEDURE_CALL] = "PROCEDURE_CALL",
                                               [PS_AST_UNARY_OPERATION] = "UNARY_OPERATION",
                                               [PS_AST_BINARY_OPERATION] = "BINARY_OPERATION",
                                               [PS_AST_FUNCTION_CALL] = "FUNCTION_CALL",
                                               [PS_AST_LITERAL_VALUE] = "VALUE",
                                               [PS_AST_RVALUE] = "RVALUE",
                                               [PS_AST_LVALUE] = "LVALUE"};

const char *ps_ast_node_get_kind_name(ps_ast_node_kind kind)
{
    static char error[16];
    unsigned int kind_value = (unsigned int)kind;
    if (kind_value > PS_AST_LVALUE)
    {
        snprintf(error, sizeof(error), "ERROR(%d)", kind_value);
        fprintf(stderr, "ERROR: Invalid AST node kind %d\n", kind_value);
        return error;
    }
    return ps_ast_node_kind_names[kind];
}

void ps_ast_debug_line(int margin, const char *format, ...) // NOSONAR
{
    if (!ps_ast_debug)
        return;
    va_list args;
    va_start(args, format);
    if (margin > 0)
        fprintf(stderr, "%*c", margin * 2, ' ');
    vfprintf(stderr, format, args); // NOSONAR
    fprintf(stderr, "\n");
    va_end(args);
}

void ps_ast_debug_word(int margin, const char *format, ...) // NOSONAR
{
    if (!ps_ast_debug)
        return;
    va_list args;
    va_start(args, format);
    if (margin > 0)
        fprintf(stderr, "%*c", margin * 2, ' ');
    vfprintf(stderr, format, args); // NOSONAR
    va_end(args);
}

void ps_ast_debug_value(int margin, const ps_ast_value *value_node)
{
    ps_ast_debug_line(margin, "{VALUE:} %s {%s: %s}", ps_value_get_display_string(&value_node->value, 0, 0),
                      ps_type_definition_get_name(value_node->value.type->value->data.t),
                      ps_value_get_debug_string(&value_node->value));
}

void ps_ast_debug_variable_simple(int margin, const ps_ast_variable *variable_simple)
{
    ps_ast_debug_line(margin, "{VARIABLE_SIMPLE} %s", variable_simple->variable->name);
}

void ps_ast_debug_variable_array(int margin, const ps_ast_variable *variable_array)
{
    ps_ast_debug_line(margin, "{%d}%s[", variable_array->dimensions, variable_array->variable->name);
    for (int i = 0; i < variable_array->dimensions; i++)
    {
        ps_ast_debug_node(margin + 1, variable_array->indexes[i]);
    }
    ps_ast_debug_line(margin, "]");
}

void ps_ast_debug_variable(int margin, const ps_ast_variable *variable)
{
    if (variable->dimensions == 0)
        ps_ast_debug_variable_simple(margin, variable);
    else
        ps_ast_debug_variable_array(margin, variable);
}

void ps_ast_debug_statement_list(int margin, const ps_ast_statement_list *statement_list)
{
    size_t count = statement_list == NULL ? 0 : statement_list->count;
    ps_ast_debug_line(margin, "BEGIN {STATEMENT_LIST: %zu statements}", count);
    if (count == 0)
    {
        ps_ast_debug_line(margin + 1, "{NO STATEMENTS}");
    }
    else
    {
        for (int i = 0; i < statement_list->count; i++)
            ps_ast_debug_node(margin + 1, statement_list->statements[i]);
    }
    ps_ast_debug_line(margin, "END {STATEMENT_LIST}");
}

void ps_ast_debug_formal_signature(int margin, const ps_formal_signature *signature)
{
    if (signature == NULL)
        return;
    ps_ast_debug_line(margin + 1, "{Signature:");
    ps_ast_debug_line(margin + 2, "     Count: %d", signature->parameter_count);
    ps_ast_debug_line(margin + 2, "      Size: %d", signature->size);
    ps_ast_debug_line(margin + 2, "    Result: %s",
                      signature->result_type == NULL
                          ? "N/A"
                          : ps_type_definition_get_name(ps_symbol_get_type_def(signature->result_type)));
    ps_ast_debug_line(margin + 2, "Parameters:");
    for (int i = 0; i < signature->parameter_count; i++)
    {
        ps_ast_debug_line(margin + 3, "%d: %s%s: %s", i + 1, signature->parameters[i].byref ? "VAR " : "",
                          signature->parameters[i].name,
                          ps_type_definition_get_name(ps_symbol_get_type_def(signature->parameters[i].type)));
    }
    ps_ast_debug_line(margin + 1, "}");
}

void ps_ast_debug_block(int margin, const ps_ast_block *block);

void ps_ast_debug_block_symbols(int margin, const ps_symbol_table *symbol_table)
{
    const ps_symbol *symbol = NULL;
    const ps_executable *executable = NULL;

    if (symbol_table == NULL)
        return;
    for (int i = 0; i < symbol_table->table_size; i++)
    {
        if (symbol_table->buckets[i] == NULL)
            continue;
        for (int j = 0; j < symbol_table->buckets[i]->used; j++)
        {
            symbol = symbol_table->buckets[i]->symbols[j];
            if (symbol->kind == PS_SYMBOL_KIND_VARIABLE)
            {
                const ps_type_definition *type_definition = ps_symbol_get_type_def(symbol);
                ps_ast_debug_line(margin, "VAR %s: %s;", symbol->name, ps_type_definition_get_name(type_definition));
            }
            if (symbol->kind == PS_SYMBOL_KIND_PROCEDURE || symbol->kind == PS_SYMBOL_KIND_FUNCTION)
            {
                executable = symbol->value->data.x;
                ps_ast_debug_block(margin + 1, executable->block);
            }
        }
    }
}

void ps_ast_debug_block(int margin, const ps_ast_block *block)
{
    if (block->kind == PS_AST_PROGRAM)
        ps_ast_debug_line(margin, "PROGRAM %s;", block->name);
    else
        ps_ast_debug_line(margin, "%s %s", ps_ast_node_get_kind_name(block->kind), block->name);
    ps_ast_debug_formal_signature(margin, block->signature);
    if (block->kind != PS_AST_PROGRAM)
        ps_ast_debug_line(margin, ";");
    ps_ast_debug_block_symbols(margin, block->symbols);
    ps_ast_debug_statement_list(margin, block->statement_list);
    if (block->kind == PS_AST_PROGRAM)
        ps_ast_debug_line(margin, ". {%s}", block->name);
}

void ps_ast_debug_assignment(int margin, const ps_ast_assignment *assignment)
{
    assert(assignment != NULL);
    assert(assignment->kind == PS_AST_ASSIGNMENT);
    assert(assignment->lvalue != NULL);
    assert(assignment->lvalue->kind == PS_AST_LVALUE);

    ps_ast_variable *variable = assignment->lvalue;
    if (variable->dimensions == 0)
    {
        ps_ast_debug_line(margin, "%s :=", variable->variable->name);
    }
    else
    {
        ps_ast_debug_line(margin, "%s[", variable->variable->name);
        for (int i = 0; i < variable->dimensions; i++)
        {
            ps_ast_debug_node(margin + 1, variable->indexes[i]);
        }
        ps_ast_debug_line(margin, "] :=");
    }
    ps_ast_debug_node(margin + 1, assignment->expression);
}

void ps_ast_debug_unary_operation(int margin, const ps_ast_unary_operation *unary_operation)
{
    // Sonar does not like imbricated ternaries...
    char *tmp = NULL;
    switch (unary_operation->operator)
    {
    case PS_OP_NEG:
        tmp = "-";
        break;
    case PS_OP_NOT:
        tmp = "NOT";
        break;
    default:
        tmp = "???";
    }
    ps_ast_debug_line(margin, "{%s, %s}(%s", ps_operator_unary_get_name(unary_operation->operator),
                      unary_operation->result_type->name, tmp);
    ps_ast_debug_node(margin + 1, unary_operation->operand);
    ps_ast_debug_line(margin, ")");
}

void ps_ast_debug_binary_operation(int margin, const ps_ast_binary_operation *binary_operation)
{
    ps_ast_debug_line(margin, "(");
    ps_ast_debug_node(margin + 1, binary_operation->left);
    ps_ast_debug_line(margin, "%s", ps_operator_binary_get_name(binary_operation->operator));
    ps_ast_debug_node(margin + 1, binary_operation->right);
    ps_ast_debug_line(margin, ")");
}

void ps_ast_debug_if(int margin, const ps_ast_if *if_statement)
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

void ps_ast_debug_while(int margin, const ps_ast_while *while_statement)
{
    ps_ast_debug_line(margin, "WHILE");
    ps_ast_debug_node(margin + 1, while_statement->condition);
    ps_ast_debug_statement_list(margin, while_statement->body);
}

void ps_ast_debug_repeat(int margin, const ps_ast_repeat *repeat_statement)
{
    ps_ast_debug_line(margin, "REPEAT");
    ps_ast_debug_statement_list(margin + 1, repeat_statement->body);
    ps_ast_debug_line(margin, "UNTIL");
    ps_ast_debug_node(margin + 1, repeat_statement->condition);
}

void ps_ast_debug_for(int margin, const ps_ast_for *for_statement)
{
    ps_ast_debug_line(margin, "FOR");
    ps_ast_debug_variable(margin + 1, for_statement->variable);
    ps_ast_debug_line(margin, ":=");
    ps_ast_debug_node(margin + 1, for_statement->start);
    ps_ast_debug_line(margin, "%s", for_statement->downto ? "DOWNTO" : "TO");
    ps_ast_debug_node(margin + 1, for_statement->end);
    ps_ast_debug_line(margin, "DO");
    ps_ast_debug_statement_list(margin, for_statement->body);
}

static void ps_ast_debug_call(int margin, const ps_ast_call *call)
{
    if (call->executable->kind == PS_SYMBOL_KIND_PROCEDURE)
        ps_ast_debug_line(margin, "{PROCEDURE_CALL} %s(", call->executable->name);
    else
        ps_ast_debug_line(margin, "{FUNCTION_CALL} %s(", call->executable->name);
    for (int i = 0; i < call->n_args; i++)
    {
        ps_ast_debug_node(margin + 1, call->args[i]);
    }
    ps_ast_debug_line(margin, ")");
}

void ps_ast_debug_procedure_call(int margin, const ps_ast_call *call)
{
    ps_ast_debug_call(margin, call);
}

void ps_ast_debug_function_call(int margin, const ps_ast_call *call)
{
    ps_ast_debug_call(margin, call);
}

void ps_ast_debug_node(int margin, const ps_ast_node *node)
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
    case PS_AST_LITERAL_VALUE:
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
    case PS_AST_RVALUE:
    case PS_AST_LVALUE:
        ps_ast_debug_variable(margin, (const ps_ast_variable *)node);
        break;
    default:
        ps_ast_debug_line(margin, "Error: unknown AST node kind %d", node->kind);
    }
}
