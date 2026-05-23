/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_parse_declaration.h"
#include "ps_compiler.h"
#include "ps_executable.h"
#include "ps_memory.h"
#include "ps_parse.h"
#include "ps_parse_statement.h"
#include "ps_system.h"
#include "ps_token.h"

/**
 * Parse/skip program parameters
 *      [ '(' [ IDENTIFIER [ ',' IDENTIFIER ]* ] ')']
 */
static bool ps_parse_program_parameters(ps_compiler *compiler)
{
    PARSE_BEGIN("PROGRAM", "PARAMETERS")

    // Empty list?
    if (lexer->current_token.type == PS_TOKEN_LEFT_PARENTHESIS)
    {
        READ_NEXT_TOKEN
        if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
        {
            READ_NEXT_TOKEN
            PARSE_END("OK")
        }
    }
    bool loop = true;
    do
    {
        EXPECT_TOKEN(PS_TOKEN_IDENTIFIER)
        READ_NEXT_TOKEN
        switch (lexer->current_token.type)
        {
        case PS_TOKEN_COMMA:
            READ_NEXT_TOKEN
            break;
        case PS_TOKEN_RIGHT_PARENTHESIS:
            READ_NEXT_TOKEN
            loop = false;
            break;
        default:
            RETURN_ERROR(PS_ERROR_EXPECTED_IDENTIFIER)
        }
    } while (loop);

    PARSE_END("OK")
}

/**
 * Parse program declaration:
 *      PROGRAM IDENTIFIER [ '(' [ IDENTIFIER [ ',' IDENTIFIER ]* ] ')'] ';'
 *      BLOCK '.'
 */
bool ps_parse_program(ps_compiler *compiler, ps_ast_node *ast)
{
    PARSE_BEGIN("PROGRAM", "")

    ps_identifier identifier = {0};
    ps_symbol *symbol_program = NULL;

    // 'PROGRAM'
    EXPECT_TOKEN(PS_TOKEN_PROGRAM)
    uint16_t start_line = lexer->start_line;
    uint16_t start_column = lexer->start_column;
    READ_NEXT_TOKEN

    // IDENTIFIER
    EXPECT_TOKEN(PS_TOKEN_IDENTIFIER)
    COPY_IDENTIFIER(identifier)
    READ_NEXT_TOKEN

    // Skip optional parameters enclosed in parentheses
    if (!ps_parse_program_parameters(compiler))
        TRACE_ERROR("PARAMETERS")

    // ';'
    EXPECT_TOKEN(PS_TOKEN_SEMI_COLON)
    READ_NEXT_TOKEN

    ps_ast_block *program = ps_ast_create_block(start_line, start_column, NULL, PS_AST_PROGRAM, identifier);
    if (NULL == program)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    // Register program in symbol table of new environment
    // if (!ps_compiler_enter_environment(compiler, identifier))
    //     TRACE_ERROR("ENTER ENVIRONMENT")
    symbol_program = ps_symbol_alloc(PS_SYMBOL_KIND_PROGRAM, identifier, NULL);
    // if (!ps_compiler_add_symbol(compiler, symbol_program))
    //     TRACE_ERROR("ADD PROGRAM SYMBOL")

    // One "USES" clause at most after "PROGRAM"
    if (!ps_parse_uses(compiler, program))
        TRACE_ERROR("USES")

    // Block
    //  - declarations (constants, types, procedures & functions)
    //  - compound statement
    if (!ps_parse_block(compiler, program))
        TRACE_ERROR("BLOCK");
    // if (!ps_compiler_exit_environment(compiler))
    //     TRACE_ERROR("EXIT ENVIRONMENT");

    // Expect '.' at the end of program declaration
    // NB: text after '.' is not analyzed and has not to be
    EXPECT_TOKEN(PS_TOKEN_DOT)

    ast = (ps_ast_node *)program;
    PARSE_END("OK")
}

/**
 * Parse/skip uses clause (module names after USES)
 */
bool ps_parse_uses(ps_compiler *compiler, ps_ast_block *block)
{
    PARSE_BEGIN("USES", "")

    if (lexer->current_token.type == PS_TOKEN_USES)
    {
        READ_NEXT_TOKEN
        bool loop = true;
        do
        {
            if (lexer->current_token.type != PS_TOKEN_IDENTIFIER)
                RETURN_ERROR(PS_ERROR_EXPECTED_IDENTIFIER)
            READ_NEXT_TOKEN
            switch (lexer->current_token.type)
            {
            case PS_TOKEN_COMMA:
                READ_NEXT_TOKEN
                break;
            case PS_TOKEN_SEMI_COLON:
                READ_NEXT_TOKEN
                loop = false;
                break;
            default:
                RETURN_ERROR(PS_ERROR_EXPECTED_IDENTIFIER)
            }
        } while (loop);
    }

    PARSE_END("OK")
}

/**
 * Parse block:
 *      [
 *          CONST ...
 *          TYPE ...
 *          VAR ...
 *          PROCEDURE ...
 *          FUNCTION ...
 *      ]*
 *      COMPOUND_STATEMENT
 * NB: ; or . or whatever after END is analyzed by the caller
 */
bool ps_parse_block(ps_compiler *compiler, ps_ast_block *block)
{
    PARSE_BEGIN("BLOCK", "")

    bool loop = true;
    do
    {
        switch (lexer->current_token.type)
        {
        case PS_TOKEN_CONST:
            // Constants are added to block's symbol table
            if (!ps_parse_const(compiler, block))
                TRACE_ERROR("CONST")
            break;
        case PS_TOKEN_TYPE:
            // Types are added to block's symbol table
            if (!ps_parse_type(compiler, block))
                TRACE_ERROR("TYPE")
            break;
        case PS_TOKEN_VAR:
            // Variables are added to block's symbol table
            if (!ps_parse_var(compiler, block))
                TRACE_ERROR("VAR")
            break;
        case PS_TOKEN_PROCEDURE:
            RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
            // ps_ast_block *procedure =
            //     ps_ast_create_block(lexer->start_line, lexer->start_column, block, PS_AST_BLOCK, NULL);
            // if (procedure == NULL)
            //     RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
            // if (!ps_parse_procedure_or_function_declaration(compiler, procedure, PS_SYMBOL_KIND_PROCEDURE))
            //     TRACE_ERROR("PROCEDURE")
            // break;
        case PS_TOKEN_FUNCTION:
            RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
            // ps_ast_block *function =
            //     ps_ast_create_block(lexer->start_line, lexer->start_column, block, PS_AST_BLOCK, NULL);
            // if (function == NULL)
            //     RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
            // if (!ps_parse_procedure_or_function_declaration(compiler, function, PS_SYMBOL_KIND_FUNCTION))
            //     TRACE_ERROR("FUNCTION")
            // break;
        case PS_TOKEN_BEGIN:
            loop = false;
            break;
        default:
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        }
    } while (loop);

    if (!ps_parse_compound_statement(compiler, block))
        TRACE_ERROR("COMPOUND")

    PARSE_END("OK")
}

/**
 * Parse constant declaration:
 *  CONST
 *      [ IDENTIFIER '=' [ '-' ] IDENTIFIER | VALUE ';' ]+
 *      VALUE = INTEGER | REAL | UNSIGNED | CHAR | STRING | BOOLEAN
 *  Examples:
 *      Const
 *          Foo          = 42;
 *          Bar          = -3.14;
 *          Baz          = True;
 *          Hello        = 'Hello, World!';
 *          ImageWidth   = 320;
 *          ImageHeight  = 200;
 *          ImageDepth   = 8;
 *          MaxWord      = $FFFF;
 *          AllRights    = &777;
 *          DashPattern0 = %10101010;
 *          DashPattern1 = %01010101;
 * Next step:
 *      IDENTIFIER '=' IDENTIFIER | CONSTANT_EXPRESSION ';'
 * Examples:
 *      Const
 *          ImageWidth  = 320;
 *          ImageHeight = 200;
 *          ImageDepth  = 8;
 *          ImagePixels = ImageWidth * ImageHeight;
 *          ImageSize   = (ImagePixels * ImageDepth) div 8;
 * Not implemented yet in lexer:
 *          Lines       = 'First line' #10 'Second line' #10 'Third line';
 */
bool ps_parse_const(ps_compiler *compiler, ps_ast_block *block)
{
    PARSE_BEGIN("CONST", "")

    ps_identifier identifier;
    ps_value *value;
    ps_value_data data = {0};
    ps_symbol *constant;

    EXPECT_TOKEN(PS_TOKEN_CONST)
    READ_NEXT_TOKEN
    do
    {
        EXPECT_TOKEN(PS_TOKEN_IDENTIFIER)
        COPY_IDENTIFIER(identifier)
        READ_NEXT_TOKEN
        EXPECT_TOKEN(PS_TOKEN_EQ)
        READ_NEXT_TOKEN
        value = ps_value_alloc(&ps_system_none, data);
        if (value == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
        if (!ps_parse_constant_expression(compiler, value))
        {
            ps_value_free(value);
            TRACE_ERROR("CONSTANT_EXPRESSION")
        }
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN
        constant = ps_symbol_alloc(PS_SYMBOL_KIND_CONSTANT, identifier, value);
        if (constant == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
        if (!ps_compiler_add_symbol(compiler, block, constant))
            TRACE_ERROR("ADD SYMBOL")
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);

    PARSE_END("OK")
}

/**
 * @brief Parse type declaration
 * @details
 *      'TYPE' TYPE_DEFINITION ';'
 *             [ TYPE_DEFINITION ';' ]*
 */
bool ps_parse_type(ps_compiler *compiler, ps_ast_block *block)
{
    PARSE_BEGIN("TYPE", "");

    // RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
    EXPECT_TOKEN(PS_TOKEN_TYPE);
    READ_NEXT_TOKEN
    if (lexer->current_token.type != PS_TOKEN_IDENTIFIER)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    do
    {
        if (!ps_parse_type_definition(compiler, block))
            TRACE_ERROR("TYPE_DEFINITION");
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);

    PARSE_END("OK")
}

/**
 * Parse variable identifier list with commas
 */
static bool ps_parse_var_identifier_list(ps_compiler *compiler, ps_identifier *identifier, int *var_count)
{
    PARSE_BEGIN("VAR", "IDENTIFIER_LIST")

    *var_count = 0;
    do
    {
        EXPECT_TOKEN(PS_TOKEN_IDENTIFIER)
        COPY_IDENTIFIER(identifier[*var_count])
        const ps_symbol *variable = ps_compiler_find_symbol(compiler, identifier[*var_count], true);
        if (variable != NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS)
        READ_NEXT_TOKEN
        if (lexer->current_token.type == PS_TOKEN_COLON)
            break;
        if (lexer->current_token.type != PS_TOKEN_COMMA)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        READ_NEXT_TOKEN
        *var_count += 1;
        if (*var_count == 8)
            RETURN_ERROR(PS_ERROR_TOO_MANY_VARIABLES)
    } while (true);

    PARSE_END("OK")
}

/**
 * Parse variable declaration:
 *      'VAR' [ IDENTIFIER [ ',' IDENTIFIER ]* ':' TYPE ';' ]+
 *      (allow up to 8 identifiers with commas)
 * Examples:
 *     Var  a, b, c: Integer;
 *          x: Real;
 *          Name: String;
 */
bool ps_parse_var(ps_compiler *compiler, ps_ast_block *block)
{
    PARSE_BEGIN("VAR", "")

    ps_identifier identifier[8];
    int var_count;
    ps_symbol *type_symbol = NULL;

    EXPECT_TOKEN(PS_TOKEN_VAR)
    READ_NEXT_TOKEN
    do
    {
        if (!ps_parse_var_identifier_list(compiler, identifier, &var_count))
            TRACE_ERROR("VARIABLE IDENTIFIER LIST")
        READ_NEXT_TOKEN
        if (!ps_parse_type_reference(compiler, &type_symbol, NULL))
            TRACE_ERROR("TYPE REFERENCE")
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON)
        for (int i = 0; i <= var_count; i++)
        {
            if (!ps_compiler_add_variable(compiler, block, identifier[i], type_symbol))
                TRACE_ERROR("ADD VARIABLE")
        }
        READ_NEXT_TOKEN
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);

    PARSE_END("OK")
}
