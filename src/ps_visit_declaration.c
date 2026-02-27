/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_executable.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_visit.h"

/**
 * Visit program declaration:
 *      PROGRAM IDENTIFIER [ '(' [ IDENTIFIER [ ',' IDENTIFIER ]* ] ')'] ';'
 *  identifiers are ignored
 */
bool ps_visit_program(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("PROGRAM", "")

    ps_identifier identifier;
    ps_symbol *program;

    EXPECT_TOKEN(PS_TOKEN_PROGRAM)
    READ_NEXT_TOKEN
    EXPECT_TOKEN(PS_TOKEN_IDENTIFIER)
    COPY_IDENTIFIER(identifier)
    READ_NEXT_TOKEN
    // Skip optional parameters enclosed in parentheses
    if (lexer->current_token.type == PS_TOKEN_LEFT_PARENTHESIS)
    {
        do
        {
            READ_NEXT_TOKEN
            if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
                break;
            EXPECT_TOKEN(PS_TOKEN_IDENTIFIER)
            if (interpreter->debug >= DEBUG_TRACE)
            {
                ps_token_debug(stderr, "PROGRAM PARAMETER SKIPPED", &lexer->current_token);
            }
            if (lexer->current_token.type == PS_TOKEN_COMMA)
                continue;
        } while (true);
    }
    EXPECT_TOKEN(PS_TOKEN_SEMI_COLON)
    READ_NEXT_TOKEN
    if (!ps_interpreter_enter_environment(interpreter, identifier))
        TRACE_ERROR("ENTER ENVIRONMENT")
    program = ps_symbol_alloc(PS_SYMBOL_KIND_PROGRAM, identifier, NULL);
    if (!ps_interpreter_add_symbol(interpreter, program))
        TRACE_ERROR("ADD SYMBOL")
    if (!ps_visit_block(interpreter, mode))
        TRACE_ERROR("BLOCK");
    if (!ps_interpreter_exit_environment(interpreter))
        TRACE_ERROR("EXIT ENVIRONMENT");
    EXPECT_TOKEN(PS_TOKEN_DOT)
    // NB: text after '.' is not analyzed and has not to be

    VISIT_END("OK")
}

/**
 * Visit block:
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
bool ps_visit_block(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("BLOCK", "")

    bool loop = true;
    do
    {
        switch (lexer->current_token.type)
        {
        case PS_TOKEN_CONST:
            if (!ps_visit_const(interpreter, mode))
                TRACE_ERROR("CONST")
            break;
        case PS_TOKEN_TYPE:
            if (!ps_visit_type(interpreter, mode))
                TRACE_ERROR("TYPE")
            break;
            RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
        case PS_TOKEN_VAR:
            if (!ps_visit_var(interpreter, mode))
                TRACE_ERROR("VAR")
            break;
        case PS_TOKEN_PROCEDURE:
            if (!ps_visit_procedure_or_function_declaration(interpreter, MODE_SKIP, PS_SYMBOL_KIND_PROCEDURE))
                TRACE_ERROR("PROCEDURE")
            break;
        case PS_TOKEN_FUNCTION:
            if (!ps_visit_procedure_or_function_declaration(interpreter, MODE_SKIP, PS_SYMBOL_KIND_FUNCTION))
                TRACE_ERROR("FUNCTION")
            break;
        case PS_TOKEN_BEGIN:
            loop = false;
            break;
        default:
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        }
    } while (loop);

    if (!ps_visit_compound_statement(interpreter, mode))
        TRACE_ERROR("COMPOUND")

    VISIT_END("OK")
}

/**
 * Visit constant declaration:
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
bool ps_visit_const(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("CONST", "")

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
        if (!ps_visit_constant_expression(interpreter, mode, value))
        {
            ps_value_free(value);
            TRACE_ERROR("CONSTANT_EXPRESSION")
        }
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN
        constant = ps_symbol_alloc(PS_SYMBOL_KIND_CONSTANT, identifier, value);
        if (constant == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
        if (!ps_interpreter_add_symbol(interpreter, constant))
            TRACE_ERROR("ADD SYMBOL")
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);

    VISIT_END("OK")
}

/**
 * Visit variable declaration:
 *      'VAR' [ IDENTIFIER [ ',' IDENTIFIER ]* ':' TYPE ';' ]+
 *      (allow up to 8 identifiers with commas)
 * Examples:
 *     Var  a, b, c: Integer;
 *          x: Real;
 *          Name: String;
 */
bool ps_visit_var(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("VAR", "")

    ps_identifier identifier[8];
    int var_count;
    ps_symbol *type = NULL;
    ps_value_data data = {0};
    const ps_symbol *variable = NULL;

    EXPECT_TOKEN(PS_TOKEN_VAR)
    READ_NEXT_TOKEN
    do
    {
        var_count = 0;
        do
        {
            EXPECT_TOKEN(PS_TOKEN_IDENTIFIER)
            COPY_IDENTIFIER(identifier[var_count])
            variable = ps_interpreter_find_symbol(interpreter, &identifier[var_count], true);
            if (variable != NULL)
                RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS)
            READ_NEXT_TOKEN
            if (lexer->current_token.type == PS_TOKEN_COLON)
                break;
            if (lexer->current_token.type != PS_TOKEN_COMMA)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
            READ_NEXT_TOKEN
            var_count += 1;
            if (var_count == 8)
                RETURN_ERROR(PS_ERROR_TOO_MANY_VARIABLES)
        } while (true);
        READ_NEXT_TOKEN
        if (!ps_visit_type_reference(interpreter, mode, &type))
            TRACE_ERROR("TYPE REFERENCE")
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON)
        for (int i = 0; i <= var_count; i++)
            if (!ps_interpreter_add_variable(interpreter, identifier[i], type, data))
                TRACE_ERROR("ADD VARIABLE")
        READ_NEXT_TOKEN
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);

    VISIT_END("OK")
}
