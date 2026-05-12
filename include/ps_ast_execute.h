/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_AST_EXECUTE_H
#define _PS_AST_EXECUTE_H

#include "ps_ast.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Run a Pascal program */
    bool ps_ast_run_program(ps_interpreter *interpreter, const ps_ast_block *program);
    /** @brief Run a Pascal procedure */
    bool ps_ast_run_procedure(ps_interpreter *interpreter, const ps_ast_block *procedure);
    /** @brief Run a Pascal function */
    bool ps_ast_run_function(ps_interpreter *interpreter, const ps_ast_block *function);
    /** @brief Run a Pascal block */
    bool ps_ast_run_block(ps_interpreter *interpreter, const ps_ast_block *block);
    /** @brief Run a list of Pascal statements */
    bool ps_ast_run_statement_list(ps_interpreter *interpreter, const ps_ast_statement_list *statement_list);
    /** @brief Run a Pascal statement */
    bool ps_ast_run_statement(ps_interpreter *interpreter, const ps_ast_node *statement);
    /** @brief Run a Pascal assignment */
    bool ps_ast_run_assignment(ps_interpreter *interpreter, const ps_ast_assignment *assignment);
    /** @brief Run a Pascal if statement */
    bool ps_ast_run_if(ps_interpreter *interpreter, const ps_ast_if *if_statement);
    /** @brief Run a Pascal while statement */
    bool ps_ast_run_while(ps_interpreter *interpreter, const ps_ast_while *while_statement);
    /** @brief Run a Pascal repeat statement */
    bool ps_ast_run_repeat(ps_interpreter *interpreter, const ps_ast_repeat *repeat_statement);
    /** @brief Run a Pascal for statement */
    bool ps_ast_run_for(ps_interpreter *interpreter, const ps_ast_for *for_statement);
    /** @brief Run a Pascal procedure call */
    bool ps_ast_run_procedure_call(ps_interpreter *interpreter, const ps_ast_call *procedure_call);
    /** @brief Run a Pascal function call */
    bool ps_ast_run_function_call(ps_interpreter *interpreter, const ps_ast_call *function_call, ps_ast_value *result);
    /** @brief Evaluate a Pascal expression */
    bool ps_ast_eval_expression(ps_interpreter *interpreter, const ps_ast_node *expression, ps_ast_value *result);

#ifdef __cplusplus
}
#endif

#endif /* _PS_AST_EXECUTE_H */
