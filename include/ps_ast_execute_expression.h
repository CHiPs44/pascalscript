/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_AST_EXECUTE_EXPRESSION_H
#define _PS_AST_EXECUTE_EXPRESSION_H

#include "ps_ast.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Evaluate an expression */
    bool ps_ast_evaluate_expression(ps_interpreter *interpreter, const ps_ast_node *expression, ps_ast_value *result);

    /** @brief Execute function call (needed for expression evaluation) */
    bool ps_ast_execute_function_call(ps_interpreter *interpreter, const ps_ast_call *function_call,
                                      ps_ast_value *result);

#ifdef __cplusplus
}
#endif

#endif /* _PS_AST_EXECUTE_EXPRESSION_H */
