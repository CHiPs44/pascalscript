/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_AST_DEBUG_H
#define _PS_AST_DEBUG_H

#include <stdarg.h>
#include <stdbool.h>

#include "ps_ast.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Global flag to enable/disable AST debug output */
    extern bool ps_ast_debug;

    /** @brief Get the string representation of an AST node group */
    const char *ps_ast_node_get_group_name(ps_ast_node_group group);

    /** @brief Get the string representation of an AST node kind */
    const char *ps_ast_node_get_kind_name(ps_ast_node_kind kind);

    /**
     * @brief Print a debug line with optional margin
     * @note This function is only active if ps_ast_debug is true
     */
    void ps_ast_debug_line(size_t margin, const char *format, ...); // NOSONAR

    /**
     * @brief Print a debug "word" without line break
     * @note This function is only active if ps_ast_debug is true
     */
    void ps_ast_debug_word(size_t margin, const char *format, ...); // NOSONAR

    /** @brief Debug print information about any AST node */
    void ps_ast_debug_node(size_t margin, const ps_ast_node *node);

#ifdef __cplusplus
}
#endif

#endif /* _PS_AST_DEBUG_H */
