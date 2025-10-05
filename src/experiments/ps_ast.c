/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>

#include "ps_ast.h"

#define PS_AST_CREATE_NODE(KIND, TYPE, COUNT)             \
    if (kind == KIND)                                     \
    {                                                     \
        TYPE *node = (TYPE *)ps_memory_calloc(COUNT, sizeof(TYPE)); \
        if (node != NULL)                                 \
            node->kind = kind;                            \
        return node;                                      \
    }

ps_ast_node *ps_ast_create_node(ps_ast_node_kind kind, size_t count)
{
    // clang-format off
    PS_AST_CREATE_NODE(PS_AST_PROGRAM           , ps_ast_node_program           , count)
    PS_AST_CREATE_NODE(PS_AST_TYPE              , ps_ast_node_type              , count)
    PS_AST_CREATE_NODE(PS_AST_STATEMENT         , ps_ast_node_statement         , count)
    PS_AST_CREATE_NODE(PS_AST_ASSIGNMENT        , ps_ast_node_assignment        , count)
    PS_AST_CREATE_NODE(PS_AST_EXPRESSION        , ps_ast_node_expression        , count)
    PS_AST_CREATE_NODE(PS_AST_UNARY_OPERATION   , ps_ast_node_unary_operation   , count)
    PS_AST_CREATE_NODE(PS_AST_BINARY_OPERATION  , ps_ast_node_binary_operation  , count)
    PS_AST_CREATE_NODE(PS_AST_VALUE             , ps_ast_node_value             , count)
    // PS_AST_CREATE_NODE(PS_AST_, ps_ast_node_, count)
    // clang-format on
    fprintf(stderr, "ps_ast_create_node: unknown kind %d!\n", kind);
    return NULL;
}

bool ps_ast_free_node(ps_ast_node *node)
{
    switch (node->kind)
    {
    case PS_AST_PROGRAM:
        return ps_ast_free_program((ps_ast_node_program *)node);
    case PS_AST_ASSIGNMENT:
        return ps_ast_free_assignment((ps_ast_node_assignment *)node);
    default:
        fprintf(stderr, "ps_ast_free_node: unknown kind %d!\n", node->kind);
    }
    ps_memory_free(node);
}

bool ps_ast_visit_node(ps_ast_node *node)
{
    switch (node->kind)
    {
    case PS_AST_PROGRAM:
        return ps_ast_visit_program((ps_ast_node_program *)node);
    case PS_AST_ASSIGNMENT:
        return ps_ast_visit_assignment((ps_ast_node_assignment *)node);
    // ...
    default:
        fprintf(stderr, "ps_ast_visit_node: unknown kind %d!\n", node->kind);
    }
    return false;
}

ps_ast_node_program *ps_ast_create_program(size_t n_consts, size_t n_types, size_t n_vars, size_t n_statements)
{
    ps_ast_node_program *node = ps_ast_create_node(PS_AST_PROGRAM, 1);
    if (node == NULL)
        return NULL;
    node->n_consts = n_consts;
    node->consts = ps_memory_calloc(n_consts, sizeof(ps_symbol));
    if (node->consts==NULL)
    {
        ps_ast_free_program(node);
        return NULL;
    }
    node->n_types = 0;  // n_types;
    node->types = NULL; // ps_memory_calloc(n_, sizeof(???));
    node->n_vars = n_vars;
    node->vars = ps_memory_calloc(n_vars, sizeof(ps_symbol));
    if (node->vars==NULL)
    {
        ps_ast_free_program(node);
        return NULL;
    }
    node->n_statements = n_statements;
    node->statements = ps_memory_calloc(n_statements, sizeof(ps_ast_node_statement));
    if (node->statements==NULL)
    {
        ps_ast_free_program(node);
        return NULL;
    }
    return node;
}

bool ps_ast_free_program(ps_ast_node_program *node)
{
    // TODO? cleanup symbols?
    ps_memory_free(node->consts);
    // ps_memory_free(node->types);
    // TODO? cleanup symbols?
    ps_memory_free(node->vars);
    // cleanup statements
    for (size_t i = 0; i < node->n_statements; i++)
        ps_ast_free_node(&node->statements[i]);
    ps_memory_free(node->statements);
    return true;
}

bool ps_ast_visit_program(ps_ast_node_program *node)
{
    printf("PROGRAM %s;\n", node->name->name);
    if (node->n_consts > 0)
    {
        printf("\nCONST\n");
        for (size_t i = 0; i < node->n_consts; i++)
            printf("\t%s\n", ps_symbol_dump_value(&node->consts[i]));
        printf("\n");
    }
    // TODO types
    if (node->n_vars > 0)
    {
        printf("\nVAR\n");
        for (size_t i = 0; i < node->n_vars; i++)
            printf("\t%s\n", ps_symbol_dump_value(&node->vars[i]));
        printf("\n");
    }
    printf("\nBEGIN\n");
    for (size_t i = 0; i < node->n_statements; i++)
        ps_ast_visit_node(&node->statements[i]);
    printf("\nEND.\n");

    return false;
}