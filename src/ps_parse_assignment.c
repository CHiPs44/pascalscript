/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

#include "ps_array.h"
#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_compiler.h"
#include "ps_error.h"
#include "ps_parse.h"
#include "ps_parse_expression.h"
#include "ps_parse_statement.h"
#include "ps_symbol.h"
#include "ps_type_definition.h"

static bool ps_parse_array_lvalue(ps_compiler *compiler, ps_ast_block *block, ps_ast_block *owner, ps_symbol *variable,
                                  ps_ast_variable **lvalue)
{
    PARSE_BEGIN("ASSIGNMENT", "ARRAY")

    // Check array dimensions
    int dimensions = ps_array_get_dimensions(variable->value->type);
    if (dimensions == 0)
        RETURN_ERROR(PS_ERROR_INVALID_PARAMETERS)
    if (dimensions > PS_ARRAY_MAX_DIMENSIONS)
        RETURN_ERROR(PS_ERROR_TOO_MANY_DIMENSIONS)
    ps_ast_node *indexes[dimensions];
    int index_count = 0;
    const char *trace_error = NULL;
    for (int i = 0; i < dimensions; i++)
        indexes[i] = NULL;

    // Parse indexes enclosed in '[' and ']', separated by ','
    EXPECT_TOKEN_OR_GOTO_CLEANUP(PS_TOKEN_LEFT_BRACKET)
    READ_NEXT_TOKEN_OR_GOTO_CLEANUP
    int dimension = 0;
    do
    {
        // At least one index
        ps_ast_node *index = NULL;
        if (!ps_parse_expression(compiler, block, &index))
        {
            trace_error = "INDEX";
            goto cleanup;
        }
        indexes[dimension] = index;
        index_count += 1;
        dimension += 1;
        // ',' begins another index
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            // Too many indexes?
            if (dimension >= dimensions)
                GOTO_CLEANUP(PS_ERROR_TOO_MANY_DIMENSIONS)
            READ_NEXT_TOKEN_OR_GOTO_CLEANUP
            continue;
        }
        // ']' ends indexes (and loop)
        if (lexer->current_token.type == PS_TOKEN_RIGHT_BRACKET)
        {
            // Not enough indexes?
            if (dimension != dimensions)
                GOTO_CLEANUP(PS_ERROR_NOT_ENOUGH_DIMENSIONS)
            READ_NEXT_TOKEN_OR_GOTO_CLEANUP
            break;
        }
        GOTO_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN)
    } while (true);

    // Create left part of assignment
    ps_ast_variable *ast_variable = ps_ast_create_variable_array(start_line, start_column, owner, PS_AST_LVALUE,
                                                                 variable, dimensions, (ps_ast_node **)(&indexes));
    if (ast_variable == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
    *lvalue = ast_variable;

    PARSE_END("OK")

cleanup:
    for (int i = 0; i < index_count; i++)
        ps_ast_free_node(indexes[i]);
    if (trace_error != NULL)
        TRACE_ERROR(trace_error)
    return false;
}

/**
 * Parse assignment:
 *  Simple:
 *      IDENTIFIER := EXPRESSION
 *  Array access:
 *      IDENTIFIER '[' EXPRESSION [ ',' EXPRESSION ]* ']' := EXPRESSION
 * Next steps:
 *  Pointer dereference:
 *      IDENTIFIER '^' = EXPRESSION
 *      IDENTIFIER '[' EXPRESSION [ ',' EXPRESSION ]* ']' '^' := EXPRESSION
 *  Record access:
 *      IDENTIFIER '.' IDENTIFIER := EXPRESSION
 *     IDENTIFIER '[' EXPRESSION [ ',' EXPRESSION ]* ']' '.' IDENTIFIER := EXPRESSION
 * Pointer dereference + record access:
 *      IDENTIFIER '^' '.' IDENTIFIER := EXPRESSION
 *      IDENTIFIER '[' EXPRESSION [ ',' EXPRESSION ]* ']' '^' '.' IDENTIFIER := EXPRESSION
 */
bool ps_parse_assignment(ps_compiler *compiler, ps_ast_block *block, ps_ast_assignment **assignment_ptr,
                         ps_ast_block *owner, ps_symbol *variable)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(assignment_ptr != NULL);
    assert(variable != NULL);

    PARSE_BEGIN("STATEMENT", "ASSIGNMENT")

    ps_ast_variable *lvalue = NULL;
    ps_ast_node *rvalue = NULL;
    const char *trace_error = NULL;

    // IDENTIFIER
    if (variable->kind == PS_SYMBOL_KIND_CONSTANT)
    {
        ps_compiler_set_error_message(compiler, PS_ERROR_ASSIGN_TO_CONST, "Constant '%s' cannot be assigned",
                                      variable->name);
        TRACE_ERROR("CONSTANT!");
    }
    if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
    {
        ps_compiler_set_error_message(compiler, PS_ERROR_EXPECTED_VARIABLE, "Symbol '%s' is not a variable",
                                      variable->name);
        TRACE_ERROR("VARIABLE!");
    }

    if (compiler->debug >= PS_DEBUG_VERBOSE)
        fprintf(stderr, "\nINFO\tASSIGNMENT: #1 variable '%s' type is '%s'\n", variable->name,
                ps_type_definition_get_name(variable->value->type->value->data.t));
    if (ps_value_get_type(variable->value) == PS_TYPE_ARRAY)
    {
        // => array_var[index(, index)]
        if (!ps_parse_array_lvalue(compiler, block, owner, variable, &lvalue))
            TRACE_ERROR("ARRAY")
    }
    else
    {
        lvalue = ps_ast_create_variable_simple(start_line, start_column, owner, PS_AST_LVALUE, variable);
        if (lvalue == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
    }

    // ':='
    EXPECT_TOKEN_OR_GOTO_CLEANUP(PS_TOKEN_ASSIGN);
    READ_NEXT_TOKEN_OR_GOTO_CLEANUP
    ps_ast_debug_line(1, "DEBUG\tParsing assignment to variable '%s' of type '%s'", variable->name,
                      ps_type_definition_get_name(variable->value->type->value->data.t));

    // RVALUE / EXPRESSION
    if (!ps_parse_expression(compiler, block, &rvalue))
    {
        trace_error = "EXPRESSION1";
        goto cleanup;
    }

    // TODO check if rvalue type matches lvalue type

    // Create assignement
    ps_ast_assignment *assignment = ps_ast_create_assignment(start_line, start_column, lvalue, rvalue);
    if (assignment == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)
    *assignment_ptr = assignment;
    lvalue = NULL;
    rvalue = NULL;
    PARSE_END("OK")

cleanup:
    ps_ast_free_node((ps_ast_node *)lvalue);
    ps_ast_free_node(rvalue);
    if (trace_error != NULL)
        TRACE_ERROR(trace_error)
    return false;
}
