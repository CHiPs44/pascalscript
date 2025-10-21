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
 */
bool ps_visit_program(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("PROGRAM", "");

    ps_identifier identifier;
    ps_symbol *program;

    EXPECT_TOKEN(PS_TOKEN_PROGRAM);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
    COPY_IDENTIFIER(identifier);
    READ_NEXT_TOKEN;
    // Skip optional parameters enclosed in parentheses
    if (lexer->current_token.type == PS_TOKEN_LEFT_PARENTHESIS)
    {
        do
        {
            READ_NEXT_TOKEN;
            if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
                break;
            EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
            if (lexer->current_token.type == PS_TOKEN_COMMA)
                continue;
        } while (true);
    }
    EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
    READ_NEXT_TOKEN;
    if (!ps_interpreter_enter_environment(interpreter, &identifier))
        RETURN_ERROR(interpreter->error);
    program = ps_symbol_alloc(PS_SYMBOL_KIND_PROGRAM, &identifier, NULL);
    if (!ps_interpreter_add_symbol(interpreter, program))
        RETURN_ERROR(interpreter->error);
    if (!ps_visit_block(interpreter, mode))
        TRACE_ERROR("BLOCK");
    ps_interpreter_exit_environment(interpreter);
    EXPECT_TOKEN(PS_TOKEN_DOT);
    // NB: text after '.' is not analyzed and has not to be

    VISIT_END("OK");
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
    VISIT_BEGIN("BLOCK", "");

    bool loop = true;
    do
    {
        switch (lexer->current_token.type)
        {
        case PS_TOKEN_CONST:
            if (!ps_visit_const(interpreter, mode))
                TRACE_ERROR("CONST");
            break;
        case PS_TOKEN_TYPE:
            if (!ps_visit_type(interpreter, mode))
                TRACE_ERROR("TYPE");
            break;
            RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
        case PS_TOKEN_VAR:
            if (!ps_visit_var(interpreter, mode))
                TRACE_ERROR("VAR");
            break;
        case PS_TOKEN_PROCEDURE:
            if (!ps_visit_procedure_or_function(interpreter, MODE_SKIP, PS_SYMBOL_KIND_PROCEDURE))
                TRACE_ERROR("PROCEDURE");
            break;
        case PS_TOKEN_FUNCTION:
            if (!ps_visit_procedure_or_function(interpreter, MODE_SKIP, PS_SYMBOL_KIND_FUNCTION))
                TRACE_ERROR("FUNCTION");
            break;
        case PS_TOKEN_BEGIN:
            loop = false;
            break;
        default:
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        }
    } while (loop);

    if (!ps_visit_compound_statement(interpreter, mode))
        TRACE_ERROR("COMPOUND");

    VISIT_END("OK");
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
 *          Lines        = 'First line' #10 'Second line' + #10#13 + 'Third line';
 * Next step:
 *      IDENTIFIER '=' IDENTIFIER | CONSTANT_EXPRESSION ';'
 * Examples:
 *      Const
 *          ImageWidth  = 320;
 *          ImageHeight = 200;
 *          ImageDepth  = 8;
 *          ImageSize    = (ImageWidth * ImageHeight * ImageDepth) div 8;
 */
bool ps_visit_const(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("CONST", "");

    ps_identifier identifier;
    ps_symbol *type;
    ps_value *value;
    ps_value_data data;
    ps_symbol *constant;
    bool negate = false;
    ps_string *s = NULL;

    EXPECT_TOKEN(PS_TOKEN_CONST);
    READ_NEXT_TOKEN;
    do
    {
        EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
        COPY_IDENTIFIER(identifier);
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(PS_TOKEN_EQ);
        READ_NEXT_TOKEN;
        // TODO allow constant expression
        // For now keep track of '-' so "Const Foo = -4;" works as expected
        if (lexer->current_token.type == PS_TOKEN_MINUS)
        {
            negate = true;
            READ_NEXT_TOKEN;
            if (lexer->current_token.type != PS_TOKEN_IDENTIFIER &&
                lexer->current_token.type != PS_TOKEN_INTEGER_VALUE &&
                lexer->current_token.type != PS_TOKEN_REAL_VALUE &&
                lexer->current_token.type != PS_TOKEN_UNSIGNED_VALUE)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        }
        switch (lexer->current_token.type)
        {
        case PS_TOKEN_IDENTIFIER:
            constant = ps_interpreter_find_symbol(interpreter, &lexer->current_token.value.identifier, false);
            if (constant == NULL)
                RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
            if (constant->kind != PS_SYMBOL_KIND_CONSTANT)
                RETURN_ERROR(PS_ERROR_EXPECTED_CONSTANT);
            type = constant->value->type;
            data = constant->value->data;
            if (negate)
            {
                switch (type->value->type->value->data.t->base)
                {
                case PS_TYPE_INTEGER:
                    data.i = -data.i;
                    break;
                case PS_TYPE_UNSIGNED:
                    if (data.u > PS_INTEGER_MAX)
                        RETURN_ERROR(PS_ERROR_OUT_OF_RANGE);
                    type = &ps_system_integer;
                    data.i = -data.u;
                    break;
                case PS_TYPE_REAL:
                    type = &ps_system_real;
                    data.r = -data.r;
                    break;
                default:
                    RETURN_ERROR(PS_ERROR_EXPECTED_NUMBER);
                }
            }
            break;
        case PS_TOKEN_INTEGER_VALUE:
            type = &ps_system_integer;
            data.i = negate ? -lexer->current_token.value.i : lexer->current_token.value.i;
            break;
        case PS_TOKEN_REAL_VALUE:
            type = &ps_system_real;
            data.r = negate ? -lexer->current_token.value.r : lexer->current_token.value.r;
            break;
        case PS_TOKEN_UNSIGNED_VALUE:
            if (negate)
            {
                if (lexer->current_token.value.u > PS_INTEGER_MAX)
                    RETURN_ERROR(PS_ERROR_OUT_OF_RANGE);
                type = &ps_system_integer;
                data.i = -lexer->current_token.value.u;
            }
            else
            {
                type = &ps_system_unsigned;
                data.u = lexer->current_token.value.u;
            }
            break;
        case PS_TOKEN_CHAR_VALUE:
            type = &ps_system_char;
            data.c = lexer->current_token.value.c;
            break;
        case PS_TOKEN_BOOLEAN_VALUE:
            type = &ps_system_boolean;
            data.b = lexer->current_token.value.b;
            break;
        case PS_TOKEN_STRING_VALUE:
            s = NULL;
            if (mode == MODE_EXEC)
            {
                s = ps_string_heap_create(interpreter->string_heap, lexer->current_token.value.s);
                if (s == NULL)
                    RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
            }
            type = &ps_system_string;
            data.s = s;
            break;
        default:
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        }
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN;
        // if (mode == MODE_EXEC)
        // {
        value = ps_value_alloc(type, data);
        if (value == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
        constant = ps_symbol_alloc(PS_SYMBOL_KIND_CONSTANT, &identifier, value);
        if (constant == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
        if (!ps_interpreter_add_symbol(interpreter, constant))
            RETURN_ERROR(interpreter->error);
        // }
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);

    VISIT_END("OK");
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
    VISIT_BEGIN("VAR", "");

    ps_identifier identifier[8];
    int var_count;
    ps_symbol *type = NULL;
    ps_value *value = NULL;
    ps_value_data data = {0};
    ps_symbol *variable = NULL;

    EXPECT_TOKEN(PS_TOKEN_VAR);
    READ_NEXT_TOKEN;
    do
    {
        var_count = 0;
        do
        {
            EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
            COPY_IDENTIFIER(identifier[var_count]);
            variable = ps_interpreter_find_symbol(interpreter, &identifier[var_count], true);
            if (variable != NULL)
                RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS);
            READ_NEXT_TOKEN;
            if (lexer->current_token.type == PS_TOKEN_COLON)
                break;
            if (lexer->current_token.type == PS_TOKEN_COMMA)
            {
                READ_NEXT_TOKEN;
                var_count++;
                if (var_count > 8 - 1)
                    RETURN_ERROR(PS_ERROR_TOO_MANY_VARIABLES);
                continue;
            }
        } while (true);
        READ_NEXT_TOKEN;
        if (!ps_visit_type_reference(interpreter, mode, &type))
            RETURN_ERROR(interpreter->error);
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN;
        // if (mode==MODE_EXEC)
        // {
        for (int i = 0; i <= var_count; i++)
        {
            value = ps_value_alloc(type, data);
            variable = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, &identifier[i], value);
            if (variable == NULL)
                RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
            if (!ps_interpreter_add_symbol(interpreter, variable))
                RETURN_ERROR(interpreter->error);
        }
        // }
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);

    VISIT_END("OK");
}
