/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <stdio.h>

#include "ps_array.h"
#include "ps_ast.h"
#include "ps_compiler.h"
#include "ps_parse.h"
#include "ps_parse_executable.h"
#include "ps_parse_expression.h"
#include "ps_parse_read.h"
#include "ps_procedures.h"
#include "ps_symbol.h"
#include "ps_system.h"
#include "ps_token.h"

static bool ps_parse_read_array_lvalue(ps_compiler *compiler, ps_ast_block *block, ps_ast_block *owner,
                                       ps_symbol *variable, ps_ast_variable **lvalue)
{
    PARSE_BEGIN("READ", "ARRAY")

    int dimensions = ps_array_get_dimensions(variable->value->type);
    if (dimensions == 0)
        RETURN_ERROR(PS_ERROR_INVALID_PARAMETERS)
    if (dimensions > PS_ARRAY_MAX_DIMENSIONS)
        RETURN_ERROR(PS_ERROR_TOO_MANY_DIMENSIONS)

    ps_ast_node *indexes[dimensions];
    int index_count = 0;
    for (int i = 0; i < dimensions; i++)
        indexes[i] = NULL;

    EXPECT_TOKEN_OR_GOTO_CLEANUP(PS_TOKEN_LEFT_BRACKET)
    READ_NEXT_TOKEN_OR_GOTO_CLEANUP
    int dimension = 0;
    do
    {
        ps_ast_node *index = NULL;
        if (!ps_parse_expression(compiler, block, &index))
            GOTO_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN)
        indexes[dimension] = index;
        index_count += 1;
        dimension += 1;

        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            if (dimension >= dimensions)
                GOTO_CLEANUP(PS_ERROR_TOO_MANY_DIMENSIONS)
            READ_NEXT_TOKEN_OR_GOTO_CLEANUP
            continue;
        }
        if (lexer->current_token.type == PS_TOKEN_RIGHT_BRACKET)
        {
            if (dimension != dimensions)
                GOTO_CLEANUP(PS_ERROR_NOT_ENOUGH_DIMENSIONS)
            READ_NEXT_TOKEN_OR_GOTO_CLEANUP
            break;
        }
        GOTO_CLEANUP(PS_ERROR_UNEXPECTED_TOKEN)
    } while (true);

    *lvalue =
            ps_ast_create_variable_array(start_line, start_column, owner, PS_AST_LVALUE, variable, dimensions, indexes);
    if (*lvalue == NULL)
        GOTO_CLEANUP(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")

cleanup:
    for (int i = 0; i < index_count; i++)
        ps_ast_free_node(indexes[i]);
    return false;
}

static bool ps_parse_read_variable(ps_compiler *compiler, ps_ast_block *block, uint16_t *n_args,
                                   ps_ast_node *args[PS_PARAMETERS_MAX])
{
    PARSE_BEGIN("READ", "VARIABLE")

    if (*n_args >= PS_PARAMETERS_MAX)
    {
        ps_compiler_set_error_message(compiler, PS_ERROR_TOO_MANY_ARGUMENTS, "Too many arguments, maximum is %d",
                                      PS_PARAMETERS_MAX);
        TRACE_ERROR("TOO_MANY_ARGUMENTS")
    }

    ps_ast_block *owner = NULL;
    ps_symbol *variable = NULL;
    if (!ps_parse_variable_reference(compiler, block, &owner, &variable))
    {
        if (variable != NULL)
            ps_compiler_set_error_message(compiler, PS_ERROR_EXPECTED_VARIABLE, "Expected variable, got %s",
                                          ps_symbol_get_kind_name(variable->kind));
        TRACE_ERROR("EXPECTED_VARIABLE")
    }

    ps_ast_variable *variable_node = NULL;
    if (ps_value_get_type(variable->value) == PS_TYPE_ARRAY)
    {
        if (!ps_parse_read_array_lvalue(compiler, block, owner, variable, &variable_node))
            TRACE_ERROR("ARRAY")
    }
    else
    {
        variable_node = ps_ast_create_variable_simple(start_line, start_column, owner, PS_AST_LVALUE, variable);
        if (variable_node == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
    }

    args[*n_args] = (ps_ast_node *)variable_node;
    *n_args += 1;

    PARSE_END("OK")
}

static bool ps_parse_readstr_string(ps_compiler *compiler, ps_ast_block *block, uint16_t *n_args,
                                    ps_ast_node *args[PS_PARAMETERS_MAX])
{
    PARSE_BEGIN("READ", "READSTR_STRING")

    ps_ast_block *owner = NULL;
    ps_symbol *variable = NULL;
    if (!ps_parse_variable_reference(compiler, block, &owner, &variable))
    {
        if (variable != NULL)
            ps_compiler_set_error_message(compiler, PS_ERROR_EXPECTED_VARIABLE, "Expected variable, got %s",
                                          ps_symbol_get_kind_name(variable->kind));
        TRACE_ERROR("EXPECTED_VARIABLE")
    }
    if (!ps_value_is_string(variable->value))
    {
        ps_compiler_set_error_message(compiler, PS_ERROR_EXPECTED_STRING, "Expected string variable, got %s",
                                      ps_value_get_type_name(variable->value));
        TRACE_ERROR("STRING_VARIABLE")
    }

    ps_ast_variable *variable_node =
            ps_ast_create_variable_simple(start_line, start_column, owner, PS_AST_LVALUE, variable);
    if (variable_node == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    args[0] = (ps_ast_node *)variable_node;
    *n_args = 1;
    EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_COMMA)
    READ_NEXT_TOKEN_OR_RETURN_FALSE

    PARSE_END("OK")
}

/**
 * Parse
 *      'READ'  | 'READLN' | 'READSTR' [ '('
 *          variable [ ',' variable ] ]*
 *      ')' ] ;
 * Next steps:
 *   Read from text file:
 *      'READ' | 'READLN' [ '('
 *          [ file_variable ',' ]
 *          variable [ ',' variable ]*
 *      ')' ] ;
 *   Read from binary file:
 *      'READ' '('
 *          file_variable ',' variable
 *      ')' ;
 */
bool ps_parse_read_or_readln_or_readstr(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call_ptr,
                                        ps_read_write_mode mode)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(call_ptr != NULL);

    PARSE_BEGIN("STATEMENT", "READ_OR_READLN_READSTR")

    uint16_t n_args = 0;
    ps_ast_node *args[PS_PARAMETERS_MAX] = {0};
    bool loop = true;

    // "Read[Ln];" or "Read[Ln] Else|End|Until"?
    // "ReadLn" without parameters is legal and skips to the end of the line.
    if (PS_TOKEN_NONE != ps_parser_expect_statement_end_token(compiler->parser))
    {
        if (mode == PS_READ_WRITE_MODE_STR)
            RETURN_ERROR(PS_ERROR_EXPECTED_VARIABLE)
    }
    else
    {
        EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_LEFT_PARENTHESIS)
        READ_NEXT_TOKEN_OR_RETURN_FALSE

        if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
        {
            if (mode == PS_READ_WRITE_MODE_STR)
                RETURN_ERROR(PS_ERROR_EXPECTED_VARIABLE)
            READ_NEXT_TOKEN_OR_RETURN_FALSE
            loop = false;
        }
        else if (mode == PS_READ_WRITE_MODE_STR && !ps_parse_readstr_string(compiler, block, &n_args, args))
            TRACE_ERROR("READSTR_STRING")

        while (loop)
        {
            if (!ps_parse_read_variable(compiler, block, &n_args, args))
                TRACE_ERROR("VARIABLE")

            if (lexer->current_token.type == PS_TOKEN_COMMA)
                READ_NEXT_TOKEN_OR_RETURN_FALSE
            else
            {
                EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_RIGHT_PARENTHESIS)
                READ_NEXT_TOKEN_OR_RETURN_FALSE
                loop = false;
            }
        }
    }

    ps_symbol *procedure = &ps_system_procedure_read;
    if (mode == PS_READ_WRITE_MODE_LN)
        procedure = &ps_system_procedure_readln;
    else if (mode == PS_READ_WRITE_MODE_STR)
        procedure = &ps_system_procedure_readstr;

    *call_ptr = ps_ast_create_call(start_line, start_column, PS_AST_PROCEDURE_CALL, procedure, n_args,
                                   n_args > 0 ? args : NULL, NULL);
    if (*call_ptr == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")
}
