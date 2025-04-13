/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_AST_H
#define _PS_AST_H

#include "ps_symbol.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum enum_ps_ast_node_kind
    {
        PS_AST_NODE = 0,
        PS_AST_PROGRAM,
        PS_AST_CONST,
        PS_AST_TYPE,
        PS_AST_VAR,
        PS_AST_STATEMENT,
        PS_AST_ASSIGNMENT,
        PS_AST_EXPRESSION,
        PS_AST_UNARY_OPERATION,
        PS_AST_BINARY_OPERATION,
        PS_AST_VALUE,
    } ps_ast_node_kind;

    typedef struct s_ps_ast_node
    {
        ps_ast_node_kind kind;
    } ps_ast_node;

    ps_ast_node *ps_ast_create_node(ps_ast_node_kind kind, size_t count);

    typedef struct ps_ast_node_program
    {
        // clang-format off
        ps_ast_node_kind       kind;
        ps_symbol             *name;
        size_t                 n_consts;
        ps_symbol             *consts;
        size_t                 n_types;
        ps_ast_node_type      *types;
        size_t                 n_vars;
        ps_symbol             *vars;
        size_t                 n_statements;
        ps_ast_node_statement *statements;
        // clang-format on
    } ps_ast_node_program;

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
        ps_ast_node_kind kind;
        ps_symbol *var;
        ps_ast_node_expression *expression;
    } ps_ast_node_assignment;

    typedef struct
    {
        ps_ast_node_kind kind;
        ps_ast_node_statement *next;
    } ps_ast_node_expression;

    typedef enum
    {
        PS_AST_OP_NEG,
        PS_AST_OP_NOT,
    } ps_ast_node_unary_operator;

    typedef struct
    {
        ps_ast_node_kind kind;
        ps_ast_node_statement *next;
        ps_ast_node_unary_operator op;
        ps_ast_node_value arg;
    } ps_ast_node_unary_operation;

    typedef enum
    {
        // additive / terms
        PS_AST_OP_ADD,
        PS_AST_OP_SUB,
        PS_AST_OP_OR,
        PS_AST_OP_XOR,
        // multiply / factors
        PS_AST_OP_MUL,
        PS_AST_OP_DIV,
        PS_AST_OP_DIV_REAL,
        PS_AST_OP_MOD,
        PS_AST_OP_AND,
        PS_AST_OP_SHL,
        PS_AST_OP_SHR,
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
        ps_ast_node_kind kind;
        // unsigned_integer, unsigned_real, character_value, character_string
        ps_value value;
        // variable_reference, constant_reference
        ps_symbol *symbol;
    } ps_ast_node_value;

#ifdef __cplusplus
}
#endif

#endif /* _PS_AST_H */
