/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_AST_H
#define _PS_AST_H

#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum enum_ps_ast_node_kind
    {
        PS_AST_NODE = 0,
        PS_AST_BLOCK,
        PS_AST_STATEMENT,
        PS_AST_STATEMENT_LIST,
        PS_AST_ASSIGNMENT,
        PS_AST_EXPRESSION,
        PS_AST_UNARY_OPERATION,
        PS_AST_BINARY_OPERATION,
        PS_AST_FUNCTION_CALL,
        PS_AST_VALUE,
    } ps_ast_node_kind;

    typedef struct s_ps_ast_node
    {
        ps_ast_node_kind kind;
        union {
            ps_ast_node_block *block;
            ps_ast_node_assignment *assignment;
            ps_ast_node_binary_operation *binary_operation;
            ps_ast_node_expression *expression;
        }
    } ps_ast_node;

    ps_ast_node *ps_ast_create_node(ps_ast_node_kind kind, size_t count);

    typedef enum enum_ps_ast_block_kind
    {
        PS_BLOCK_PROGRAM,
        PS_BLOCK_UNIT,
        PS_BLOCK_FUNCTION,
        PS_BLOCK_PROCEDURE,
    } ps_ast_block_kind;

    typedef struct ps_ast_node_block
    {
        ps_ast_block_kind kind;
        ps_identifier name;
        size_t parameter_count;   // can be zero
        ps_parameter *parameters; // NULL if no parameters
        ps_symbol *result_type;   // for functions, NULL for procedures
        ps_symbol_table
            *symbol_table; /** @brief constants, types, variables, functions and procedures defined in this block */
        ps_ast_node_statement_list statements;
    } ps_ast_node_block;

    typedef struct ps_parameter
    {
        ps_identifier name;
        ps_symbol *type;
        bool byref : 1; // true if parameter is passed by reference (var parameter)
    } ps_parameter;

    typedef struct ps_ast_node_statement_list
    {
        size_t size;
        size_t used;
        ps_ast_node_statement **statements;
    } ps_ast_node_statement_list;

    typedef struct
    {
        // clang-format off
        ps_ast_node_kind       kind;
        // clang-format off
        // TODO
    } ps_ast_node_type;

    typedef struct
    {
        ps_ast_node_kind kind;
    } ps_ast_node_statement;

    typedef struct
    {
        ps_symbol *var;
        ps_ast_node_expression *expression;
    } ps_ast_node_assignment;

    typedef struct
    {
        ps_ast_node_statement *next;
    } ps_ast_node_expression;

    typedef enum
    {
        PS_OP_NEG,
        PS_OP_NOT,
    } ps_ast_node_unary_operator;

    typedef struct
    {
        ps_ast_node_statement *next;
        ps_ast_node_unary_operator op;
        ps_ast_node_value arg;
    } ps_ast_node_unary_operation;

    typedef enum
    {
        // additive / terms
        PS_OP_ADD,
        PS_OP_SUB,
        PS_OP_OR,
        PS_OP_XOR,
        // multiply / factors
        PS_OP_MUL,
        PS_OP_DIV,
        PS_OP_DIV_REAL,
        PS_OP_MOD,
        PS_OP_AND,
        PS_OP_SHL,
        PS_OP_SHR,
        // comparison
        PS_OP_EQ,
        PS_OP_GE,
        PS_OP_GT,
        PS_OP_LE,
        PS_OP_LT,
        PS_OP_NE,
    } ps_ast_node_binary_operator;

    typedef struct
    {
        ps_ast_node_kind kind;
        ps_ast_node_value *left;
        ps_ast_node_binary_operator operator;
        ps_ast_node_value *right;
    } ps_ast_node_binary_operation;

    typedef struct
    {
        // unsigned_integer, unsigned_real, character_value, character_string
        ps_value value;
        // variable_reference, constant_reference
        ps_symbol *symbol;
    } ps_ast_node_value;

#ifdef __cplusplus
}
#endif

#endif /* _PS_AST_H */
