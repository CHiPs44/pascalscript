/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>

#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_ast_execute.h"
#include "ps_ast_execute_expression.h"
#include "ps_ast_execute_system.h"
#include "ps_interpreter.h"
#include "ps_operator.h"
#include "ps_system.h"
#include "ps_value.h"

static bool ps_ast_evaluate_expression_literal(ps_interpreter *interpreter, const ps_ast_value *value,
                                               ps_ast_value *result)
{
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Value: %s",
                           ps_value_get_display_string(&value->value, 0, 0));
    if (!ps_interpreter_copy_value(interpreter, &value->value, &result->value))
        return false;
    return true;
}

static bool ps_ast_evaluate_expression_rvalue(ps_interpreter *interpreter, const ps_ast_variable *variable,
                                              ps_ast_value *result)
{
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Variable: %s", variable->variable->name);
    ps_value value = {.allocated = false, .type = &ps_system_none, .data = {0}};
    if (!ps_interpreter_get_variable_value(interpreter, variable, &value) ||
        !ps_interpreter_copy_value(interpreter, &value, &result->value))
    {
        return false;
    }
    return true;
}

static bool ps_ast_evaluate_expression_unary(ps_interpreter *interpreter, const ps_ast_unary_operation *unary_operation,
                                             ps_ast_value *result)
{
    ps_ast_value operand_value = {.group = PS_AST_EXPRESSION,
                                  .kind = PS_AST_LITERAL_VALUE,
                                  .value.allocated = false,
                                  .value.type = &ps_system_none,
                                  .value.data = {0}};
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Unary operation: %s",
                           ps_operator_unary_get_name(unary_operation->operator));
    if (!ps_ast_evaluate_expression(interpreter, unary_operation->operand, &operand_value) ||
        !ps_operator_unary_eval(interpreter, &operand_value.value, &result->value, unary_operation->operator))
    {
        return false;
    }
    return true;
}

static bool ps_ast_evaluate_expression_binary(ps_interpreter *interpreter,
                                              const ps_ast_binary_operation *binary_operation, ps_ast_value *result)
{
    ps_ast_value left = {.group = PS_AST_EXPRESSION,
                         .kind = PS_AST_LITERAL_VALUE,
                         .value = {.allocated = false, .type = &ps_system_none, .data = {0}}};
    if (!ps_ast_evaluate_expression(interpreter, binary_operation->left, &left))
        return false;
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "{LEFT: %s, %s}", ps_value_get_type_name(&left.value),
                           ps_value_get_debug_string(&left.value));

    ps_ast_value right = {.group = PS_AST_EXPRESSION,
                          .kind = PS_AST_LITERAL_VALUE,
                          .value = {.allocated = false, .type = &ps_system_none, .data = {0}}};
    if (!ps_ast_evaluate_expression(interpreter, binary_operation->right, &right))
        return false;
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "{RIGHT: %s, %s}", ps_value_get_type_name(&right.value),
                           ps_value_get_debug_string(&right.value));

    ps_value result_value = {.allocated = false, .type = &ps_system_none, .data = {0}};
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Binary operation: %s {%s, %s}",
                           ps_operator_binary_get_name(binary_operation->operator), ps_value_get_type_name(&left.value),
                           ps_value_get_type_name(&right.value));
    if (!ps_operator_binary_eval(interpreter, (const ps_value *)&left.value, (const ps_value *)&right.value,
                                 &result_value, binary_operation->operator))
    {
        return false;
    }
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "{RESULT: %s, %s}", ps_value_get_type_name(&result_value),
                           ps_value_get_debug_string(&result_value));
    if (!ps_interpreter_copy_value(interpreter, &result_value, &result->value))
    {
        return false;
    }
    return true;
}

static bool ps_ast_evaluate_expression_function_call(ps_interpreter *interpreter, const ps_ast_call *function_call,
                                                     ps_ast_value *result)
{
    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "Function call: %s", function_call->executable->name);
    return ps_ast_execute_function_call(interpreter, function_call, result);
}

bool ps_ast_evaluate_expression(ps_interpreter *interpreter, const ps_ast_node *expression, ps_ast_value *result)
{
    assert(expression != NULL);
    assert(expression->group == PS_AST_EXPRESSION);
    assert(result != NULL);

    if (!ps_ast_node_check_group(expression, PS_AST_EXPRESSION))
        return false;

    PS_AST_DEBUG_EXECUTION(interpreter, PS_DEBUG_VERBOSE, "EXPRESSION @%p", (const void *)expression);
    switch (expression->kind)
    {
    case PS_AST_LITERAL_VALUE:
        if (!ps_ast_evaluate_expression_literal(interpreter, (const ps_ast_value *)expression, result))
            goto error;
        return true;
    case PS_AST_RVALUE:
        if (!ps_ast_evaluate_expression_rvalue(interpreter, (const ps_ast_variable *)expression, result))
            goto error;
        return true;
    case PS_AST_UNARY_OPERATION:
        if (!ps_ast_evaluate_expression_unary(interpreter, (const ps_ast_unary_operation *)expression, result))
            goto error;
        return true;
    case PS_AST_BINARY_OPERATION:
        if (!ps_ast_evaluate_expression_binary(interpreter, (const ps_ast_binary_operation *)expression, result))
            goto error;
        return true;
    case PS_AST_FUNCTION_CALL:
        if (!ps_ast_evaluate_expression_function_call(interpreter, (const ps_ast_call *)expression, result))
            return false;
        return true;
    default:
        ps_interpreter_set_message(interpreter, "Unexpected expression kind %s (%d)\n",
                                   ps_ast_node_get_kind_name(expression->kind), expression->kind);
        goto error;
    }

error:
    interpreter->error_line = expression->line;
    interpreter->error_column = expression->column;
    return false;
}
