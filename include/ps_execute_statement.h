/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_EXECUTE_STATEMENT_H
#define _PS_EXECUTE_STATEMENT_H

#include "ps_ast.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Execute a list of Pascal statements */
    bool ps_ast_execute_statement_list(ps_interpreter *interpreter, const ps_ast_statement_list *statement_list);

    /** @brief Execute statement */
    bool ps_ast_execute_statement(ps_interpreter *interpreter, const ps_ast_node *statement);

    /** @brief Execute assignment */
    bool ps_ast_execute_assignment(ps_interpreter *interpreter, const ps_ast_assignment *assignment);

    /** @brief Execute if statement */
    bool ps_ast_execute_if(ps_interpreter *interpreter, const ps_ast_if *if_statement);

    /** @brief Execute while statement */
    bool ps_ast_execute_while(ps_interpreter *interpreter, const ps_ast_while *while_statement);

    /** @brief Execute repeat statement */
    bool ps_ast_execute_repeat(ps_interpreter *interpreter, const ps_ast_repeat *repeat_statement);

    /** @brief Execute for statement */
    bool ps_ast_execute_for(ps_interpreter *interpreter, const ps_ast_for *for_statement);

#ifdef __cplusplus
}
#endif

#endif /* _PS_EXECUTE_STATEMENT_H */
