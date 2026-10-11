/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_EXECUTE_EXPRESSION_H
#define _PS_EXECUTE_EXPRESSION_H

#include "ps_ast.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Evaluate an expression */
    bool ps_evaluate_expression(ps_interpreter *interpreter, const ps_ast_node *expression, ps_ast_value *result);

#ifdef __cplusplus
}
#endif

#endif /* _PS_EXECUTE_EXPRESSION_H */
