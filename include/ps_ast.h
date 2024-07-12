/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
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
        PS_AST_ASSIGN,
        PS_AST_EXPRESSION,
        PS_AST_NODE_FACTOR,
    } ps_ast_node_kind;

    typedef struct
    {
        ps_ast_node_kind kind;
    } ps_ast_node;

    ps_ast_node *ps_ast_create_node(ps_ast_node_kind kind);

    typedef struct
    {
        ps_ast_node_kind kind;
        ps_symbol *name;
        ps_ast_node_const *constants;
        ps_ast_node_type *types;
        ps_ast_node_var *vars;
        ps_ast_node_statement *statements;
    } ps_ast_node_program;

    typedef struct
    {
        ps_ast_node_kind kind;
        ps_ast_node_const *next;
        ps_symbol *symbol;
    } ps_ast_node_const;

    typedef struct
    {
        ps_ast_node_kind kind;
        ps_ast_node_type *next;
        // TODO
    } ps_ast_node_type;

    typedef struct
    {
        ps_ast_node_kind kind;
        ps_ast_node_var *next;
        ps_symbol *symbol;
    } ps_ast_node_var;

    typedef struct
    {
        ps_ast_node_kind kind;
        ps_ast_node_statement *next;
    } ps_ast_node_statement;

    typedef struct
    {
        ps_ast_node_kind kind;
        ps_ast_node_statement *next;
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
        ps_ast_node_factor arg;
    } ps_ast_node_unary_operation;

    typedef enum
    {
        PS_AST_OP_ADD,
        PS_AST_OP_SUB,
        PS_AST_OP_OR,
        PS_AST_OP_XOR,
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
        ps_ast_node_statement *next;
        ps_ast_node_binary_operator op;
        ps_ast_node_factor arg1;
        ps_ast_node_factor arg2;
    } ps_ast_node_unary_operation;

    typedef struct
    {
        ps_ast_node_kind kind;
        ps_ast_node_statement *next;
        // unsigned_integer, unsigned_real, character_value, character_string
        ps_value value;
        // variable_reference, constant_reference
        ps_symbol *symbol;
    } ps_ast_node_factor;

#ifdef __cplusplus
}
#endif

#endif /* _PS_AST_H */
