/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include "ps_ast.h"

ps_ast_node *ps_ast_create_node(ps_ast_node_kind kind)
{
    if (kind == PS_AST_NODE_PROGRAM)
    {
        ps_ast_node_program *node = (ps_ast_node_program *)calloc(sizeof(ps_ast_node_program), 1);
        if (node != NULL)
            node->kind = kind;
        return node;
    }
}