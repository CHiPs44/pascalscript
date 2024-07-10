/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_AST_H
#define _PS_AST_H

#include "ps_symbol.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum
    {
        PS_AST_NONE = 0,
        PS_AST_PROGRAM,
        PS_AST_CONST,
        // PS_AST_TYPE,
        PS_AST_VAR,
        PS_AST_STATEMENT,
        PS_AST_ASSIGN,
        PS_AST_EXPRESSION,
    } ps_ast_node_type;

    typedef struct
    {
        ps_ast_node_type type;
    } ps_ast_node;

    typedef struct
    {
        ps_ast_node_type type;
        ps_symbol *name;
        ps_ast_const *constants;
        // ps_ast_type *types;
        ps_ast_var *vars;
        ps_ast_statement *statements;
    } ps_ast_program;

    typedef struct
    {
        ps_ast_node_type type;
        ps_ast_const *next;
        ps_symbol *symbol;
    } ps_ast_const;

    // typedef struct
    // {
    //     ps_ast_node_type type;
    //     ps_ast_type *next;
    //     // TODO
    // } ps_ast_type;

    typedef struct
    {
        ps_ast_node_type type;
        ps_ast_var *next;
        ps_symbol *symbol;
    } ps_ast_var;

    typedef struct
    {
        ps_ast_node_type type;
        ps_ast_statement *next;
    } ps_ast_statement;

    typedef struct
    {
        ps_ast_node_type type;
        ps_ast_statement *next;
        ps_symbol *var;
        ps_ast_expression *expression;
    } ps_ast_assignment;

    typedef struct
    {
        ps_ast_node_type type;
    } ps_ast_expression;

#ifdef __cplusplus
}
#endif

#endif /* _PS_AST_H */
