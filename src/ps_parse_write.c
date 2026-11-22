/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <stdio.h>

#include "ps_ast.h"
#include "ps_compiler.h"
#include "ps_parse.h"
#include "ps_parse_expression.h"
#include "ps_parse_executable.h"
#include "ps_parse_write.h"
#include "ps_procedures.h"
#include "ps_symbol.h"
#include "ps_system.h"
#include "ps_token.h"

static inline bool ps_parse_write_format(ps_compiler *compiler, ps_ast_block *block, int16_t *width,
                                         int16_t *precision)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(width != NULL);
    assert(precision != NULL);

    PARSE_BEGIN("WRITE_OR_WRITELN", "FORMAT");
    (void)start_line;
    (void)start_column;

    *width = 0;
    *precision = 0;
    if (lexer->current_token.type == PS_TOKEN_COLON)
    {
        READ_NEXT_TOKEN_OR_RETURN_FALSE
        EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_INTEGER_VALUE)
        int16_t w = (int16_t)(lexer->current_token.value.i);
        if (w < 0 || w > 255)
            RETURN_ERROR(PS_ERROR_OUT_OF_RANGE)
        *width = w;
        READ_NEXT_TOKEN_OR_RETURN_FALSE
        if (lexer->current_token.type == PS_TOKEN_COLON)
        {
            READ_NEXT_TOKEN_OR_RETURN_FALSE
            EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_INTEGER_VALUE)
            int16_t p = (int16_t)(lexer->current_token.value.i);
            if (p < 0 || p > 255)
                RETURN_ERROR(PS_ERROR_OUT_OF_RANGE)
            *precision = p;
            READ_NEXT_TOKEN_OR_RETURN_FALSE
        }
    }

    PARSE_END("OK")
}

static bool ps_parse_writestr_string(ps_compiler *compiler, ps_ast_block *block, ps_read_write_mode mode,
                                     uint16_t *n_args, ps_ast_node *args[PS_PARAMETERS_MAX])
{
    ps_symbol *variable = NULL;
    ps_ast_variable *variable_node = NULL;
    ps_ast_block *owner = NULL;

    PARSE_BEGIN("STATEMENT", "WRITESTR_STRING");

    if (mode == PS_READ_WRITE_MODE_STR)
    {
        // STRING_VARIABLE
        //  - must be an ordinal simple variable, not an array item
        if (!ps_parse_variable_reference(compiler, block, &owner, &variable))
        {
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
        variable_node = ps_ast_create_variable_simple(start_line, start_column, owner, PS_AST_LVALUE, variable);
        if (variable_node == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
        args[0] = (ps_ast_node *)variable_node;
        *n_args = 1;
        EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_COMMA)
        READ_NEXT_TOKEN_OR_RETURN_FALSE
    }

    PARSE_END("OK")
}

static bool ps_parse_write_expression(ps_compiler *compiler, ps_ast_block *block, uint16_t *n_args,
                                      ps_ast_node *args[PS_PARAMETERS_MAX], ps_ast_format formats[PS_PARAMETERS_MAX])
{
    PARSE_BEGIN("WRITE", "EXPRESSION")

    int16_t width = 0;
    int16_t precision = 0;
    ps_ast_node *expression = NULL;

    // Check number of arguments
    if (*n_args >= PS_PARAMETERS_MAX)
    {
        ps_compiler_set_error_message(compiler, PS_ERROR_TOO_MANY_ARGUMENTS, "Too many arguments, maximum is %d",
                                      PS_PARAMETERS_MAX);
        TRACE_ERROR("TOO_MANY_ARGUMENTS")
    }

    // Parse argument value
    if (!ps_parse_expression(compiler, block, &expression))
        TRACE_ERROR("EXPRESSION")

    // Retrieve optional string/numeric format :width[:precision]
    if (!ps_parse_write_format(compiler, block, &width, &precision))
        TRACE_ERROR("FORMAT")

    // Store argument and format
    args[*n_args] = expression;
    formats[*n_args].width = width;
    formats[*n_args].precision = precision;
    *n_args += 1;

    PARSE_END("OK")
}

/**
 * Parse
 *      'WRITE' | 'WRITELN' | 'WRITESTR' [ '('
 *         [ string_variable ',' ]
 *          expression [ ':' width [ ':' precision ] ]
 *          [ ',' expression [ ':' width [ ':' precision ] ] ]*
 *      ')' ] ;
 * Next steps:
 *   Write to text file:
 *      'WRITE' | 'WRITELN' [ '('
 *          file_variable ','
 *          expression [ ':' width [ ':' precision ] ]
 *          [ ',' expression [ ':' width [ ':' precision ] ] ]*
 *      ')' ] ;
 *   Write to binary file:
 *      'WRITE' '('
 *          file_variable ',' expression
 *      ')' ;
 */
bool ps_parse_write_or_writeln_or_writestr(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call_ptr,
                                           ps_read_write_mode mode)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(call_ptr != NULL);

    PARSE_BEGIN("STATEMENT", "WRITE_OR_WRITELN_OR_WRITESTR");

    uint16_t n_args = 0;
    ps_ast_node *args[PS_PARAMETERS_MAX] = {0};
    ps_ast_format formats[PS_PARAMETERS_MAX] = {0};
    bool loop = true;

    // "Write[Ln];" or "Write[Ln] Else|End|Until"?
    // (Write without parameters is "legal" but is a no-op)
    if (PS_TOKEN_NONE != ps_parser_expect_statement_end_token(compiler->parser))
    {
        // "WriteStr" only is not "legal", it needs a string variable
        if (mode == PS_READ_WRITE_MODE_STR)
            RETURN_ERROR(PS_ERROR_EXPECTED_VARIABLE)
    }
    else
    {
        // '('
        EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_LEFT_PARENTHESIS)
        READ_NEXT_TOKEN_OR_RETURN_FALSE
        // ')' =>"Write[Ln]()"?
        if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
        {
            // "WriteStr()" is not "legal"
            if (mode == PS_READ_WRITE_MODE_STR)
                RETURN_ERROR(PS_ERROR_EXPECTED_VARIABLE)
            READ_NEXT_TOKEN_OR_RETURN_FALSE
            loop = false;
        }
        // "WriteStr"?
        if (!ps_parse_writestr_string(compiler, block, mode, &n_args, args))
            TRACE_ERROR("STRING")
        // Expressions + formats
        while (loop)
        {
            // EXPRESSION [ ':' WIDTH [ ':' PRECISION ] ]
            if (!ps_parse_write_expression(compiler, block, &n_args, args, formats))
                TRACE_ERROR("PARAMETER")
            // ',' => another argument
            if (lexer->current_token.type == PS_TOKEN_COMMA)
                READ_NEXT_TOKEN_OR_RETURN_FALSE
            else
            {
                // ')' => end of arguments
                EXPECT_TOKEN_OR_RETURN_FALSE(PS_TOKEN_RIGHT_PARENTHESIS)
                READ_NEXT_TOKEN_OR_RETURN_FALSE
                loop = false;
            }
        }
    }

    // Create AST node for this call
    ps_symbol *procedure = &ps_system_procedure_write;
    if (mode == PS_READ_WRITE_MODE_LN)
        procedure = &ps_system_procedure_writeln;
    else if (mode == PS_READ_WRITE_MODE_STR)
        procedure = &ps_system_procedure_writestr;
    *call_ptr = ps_ast_create_call(start_line, start_column, PS_AST_PROCEDURE_CALL, procedure, n_args,
                                   n_args > 0 ? args : NULL, formats);
    if (*call_ptr == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")
}
