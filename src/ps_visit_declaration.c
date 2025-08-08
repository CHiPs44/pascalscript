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
    ps_identifier identifier;
    ps_symbol *program;
    VISIT_BEGIN("PROGRAM", "");

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
    // ps_symbol_table_dump(NULL, "Before EXIT", ps_interpreter_get_environment(interpreter)->symbols);
    ps_interpreter_exit_environment(interpreter);
    EXPECT_TOKEN(PS_TOKEN_DOT);
    // NB: text after '.' is not analyzed and has not to be

    VISIT_END("OK");
}

/**
 * Visit block:
 *      [
 *         CONST ...
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
    bool loop = true;

    VISIT_BEGIN("BLOCK", "");

    TRACE_CURSOR;
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
            if (!ps_visit_procedure_or_function(interpreter, mode, PS_SYMBOL_KIND_PROCEDURE))
                TRACE_ERROR("PROCEDURE");
            // ps_symbol_table_dump(NULL, "PROCEDURE1?", ps_interpreter_get_environment(interpreter)->symbols);
            break;
        case PS_TOKEN_FUNCTION:
            if (!ps_visit_procedure_or_function(interpreter, mode, PS_SYMBOL_KIND_FUNCTION))
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
 *      [ IDENTIFIER '=' IDENTIFIER | [ '-' ] VALUE ';' ]*
 * Next step:
 *       IDENTIFIER '=' CONSTANT_EXPRESSION ';'
 */
bool ps_visit_const(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("CONST", "");

    ps_identifier identifier;
    ps_type_definition *type;
    ps_value *value;
    ps_value_data data;
    ps_symbol *constant;
    bool negate = false;

    EXPECT_TOKEN(PS_TOKEN_CONST);
    READ_NEXT_TOKEN;
    do
    {
        EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
        COPY_IDENTIFIER(identifier);
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(PS_TOKEN_EQUAL);
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
            constant = ps_interpreter_find_symbol(interpreter, &lexer->current_token.value.identifier, true);
            if (constant == NULL)
                RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
            if (constant->kind != PS_SYMBOL_KIND_CONSTANT)
                RETURN_ERROR(PS_ERROR_EXPECTED_CONSTANT);
            type = constant->value->type;
            data = constant->value->data;
            if (negate)
            {
                switch (type->base)
                {
                case PS_TYPE_INTEGER:
                    data.i = -data.i;
                    break;
                case PS_TYPE_UNSIGNED:
                    if (data.u > PS_INTEGER_MAX)
                        RETURN_ERROR(PS_ERROR_OUT_OF_RANGE);
                    type = ps_system_integer.value->data.t;
                    data.i = -data.u;
                    break;
                case PS_TYPE_REAL:
                    type = ps_system_real.value->data.t;
                    data.r = -data.r;
                    break;
                default:
                    RETURN_ERROR(PS_ERROR_EXPECTED_NUMBER);
                }
            }
            break;
        case PS_TOKEN_INTEGER_VALUE:
            type = ps_system_integer.value->data.t;
            data.i = negate ? -lexer->current_token.value.i : lexer->current_token.value.i;
            break;
        case PS_TOKEN_REAL_VALUE:
            type = ps_system_real.value->data.t;
            data.r = negate ? -lexer->current_token.value.r : lexer->current_token.value.r;
            break;
        case PS_TOKEN_UNSIGNED_VALUE:
            if (negate)
            {
                if (lexer->current_token.value.u > PS_INTEGER_MAX)
                    RETURN_ERROR(PS_ERROR_OUT_OF_RANGE);
                type = ps_system_integer.value->data.t;
                data.i = -lexer->current_token.value.u;
            }
            else
            {
                type = ps_system_unsigned.value->data.t;
                data.u = lexer->current_token.value.u;
            }
            break;
        case PS_TOKEN_CHAR_VALUE:
            type = ps_system_char.value->data.t;
            data.c = lexer->current_token.value.c;
            break;
        case PS_TOKEN_BOOLEAN_VALUE:
            type = ps_system_boolean.value->data.t;
            data.b = lexer->current_token.value.b;
            break;
        case PS_TOKEN_STRING_VALUE:
            ps_string *s = NULL;
            if (mode == MODE_EXEC)
            {
                s = ps_string_heap_create(interpreter->string_heap, lexer->current_token.value.s);
                if (s == NULL)
                    RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
            }
            type = ps_system_string.value->data.t;
            data.s = s;
            break;
        default:
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        }
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN;
        if (mode == MODE_EXEC)
        {
            value = ps_value_alloc(type, data);
            if (value == NULL)
                RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
            constant = ps_symbol_alloc(PS_SYMBOL_KIND_CONSTANT, &identifier, value);
            if (constant == NULL)
                RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
            if (!ps_interpreter_add_symbol(interpreter, constant))
                RETURN_ERROR(interpreter->error);
        }
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);

    VISIT_END("OK");
}

/**
 * Visit type definition:
 *      'INTEGER' | 'UNSIGNED' | 'REAL' | 'BOOLEAN' | 'CHAR' |
 *      'STRING' [ '[' IDENTIFIER | UNSIGNED ']' ]
 * Next steps:
 *      IDENTIFIER
 *      'SUBRANGE' = LOW '..' HIGH
 *      'ENUMERATION' = '(' IDENTIFIER [ ',' IDENTIFIER ]* ')'
 *      'POINTER'
 *      'FILE'
 *      'SET' = 'OF' TYPE
 *      'RECORD' = 'RECORD' FIELD_DEFINITION [ ';' FIELD_DEFINITION ]* 'END'
 *      'FIELD_DEFINITION' = IDENTIFIER ':' TYPE
 *      'ARRAY' = '[' SUBRANGE | IDENTIFIER [ ',' SUBRANGE | IDENTIFIER ]* ']' 'OF' TYPE
 *      'POINTER'
 *      'RECORD'
 *      'ARRAY'
 *      'FILE'
 */
bool ps_visit_type_definition(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_identifier *identifier,
                              ps_symbol *type_symbol)
{
    VISIT_BEGIN("TYPE_DEFINITION", "");
    ps_type_definition *type_def = NULL;
    ps_value_data data = {0};
    ps_value *value = NULL;
    ps_unsigned len = 0;
    ps_symbol *symbol = NULL;
    type_symbol = NULL;

    switch (lexer->current_token.type)
    {
        /* ********** Base types ********** */
    case PS_TOKEN_INTEGER:
        type_symbol = &ps_system_integer;
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_UNSIGNED:
        type_symbol = &ps_system_unsigned;
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_REAL:
        type_symbol = &ps_system_real;
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_BOOLEAN:
        type_symbol = &ps_system_boolean;
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_CHAR:
        type_symbol = &ps_system_char;
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_STRING:
        READ_NEXT_TOKEN;
        if (lexer->current_token.type == PS_TOKEN_LEFT_BRACKET)
        {
            READ_NEXT_TOKEN;
            if (lexer->current_token.type == PS_TOKEN_UNSIGNED_VALUE)
            {
                len = lexer->current_token.value.u;
            }
            else if (lexer->current_token.type == PS_TOKEN_IDENTIFIER)
            {
                symbol = ps_interpreter_find_symbol(interpreter, &lexer->current_token.value.identifier, false);
                if (symbol == NULL)
                    RETURN_ERROR(PS_ERROR_UNKOWN_IDENTIFIER);
                if (symbol->kind != PS_SYMBOL_KIND_CONSTANT)
                    RETURN_ERROR(PS_ERROR_EXPECTED_CONSTANT);
                if (symbol->value->type == ps_system_integer.value->data.t && symbol->value->data.i <= 0)
                {
                    RETURN_ERROR(PS_ERROR_EXPECTED_UNSIGNED);
                }
                else if (symbol->value->type != ps_system_unsigned.value->data.t)
                    RETURN_ERROR(PS_ERROR_EXPECTED_UNSIGNED);
                len = type_symbol->value->data.u;
            }
            else
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
            if (len < 1 || len > PS_STRING_MAX_LEN)
                RETURN_ERROR(PS_ERROR_EXPECTED_STRING_LENGTH);
            RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
            // type_def = ps_value_alloc(&ps_system_type_def, data);
        }
        else
        {
            type_symbol = &ps_system_string;
        }
        break;
        /* ********** Array ********** */
    case PS_TOKEN_ARRAY:
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
        // NB: array can be something like "ARRAY [1..10] OF (Value1, Value2, ...)"
        // and call recusively ps_visit_type_definition()
        // type = ps_system_array.value->data.t; // TODO: parse array definition
        // data.s = NULL; // TODO: allocate array
        // break;
        /* ********** Other type ********** */
    case PS_TOKEN_IDENTIFIER:
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    }

    if (mode == MODE_EXEC)
    {
        // Register new type definition in symbol table
        value = ps_value_alloc(type_def, data);
        if (value == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
        type_symbol = ps_symbol_alloc(PS_SYMBOL_KIND_TYPE_DEFINITION, identifier, value);
        if (type_symbol == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
        if (!ps_interpreter_add_symbol(interpreter, type_symbol))
            RETURN_ERROR(interpreter->error);
    }

    VISIT_END("OK");
}

/**
 * Visit
 *      'TYPE' IDENTIFIER '=' TYPE_DEFINITION ';'
 */
bool ps_visit_type(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("TYPE", "");
    ps_identifier identifier = {0};
    ps_symbol *symbol = NULL;

    EXPECT_TOKEN(PS_TOKEN_TYPE);
    READ_NEXT_TOKEN;
    do
    {
        EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
        COPY_IDENTIFIER(identifier);
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(PS_TOKEN_EQUAL);
        READ_NEXT_TOKEN;
        symbol = ps_interpreter_find_symbol(interpreter, &identifier, true);
        if (symbol != NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS);
        if (!ps_visit_type_definition(interpreter, mode, &identifier, symbol))
            TRACE_ERROR("TYPE_DEFINITION");
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN;
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);

    VISIT_END("OK");
}

/**
 * Visit    VAR IDENTIFIER : TYPE;
 *              IDENTIFIER : TYPE;
 *          ...
 * Next step: allow identifier list with commas
 *              IDENTIFIER, IDENTIFIER, ... : TYPE ;
 */
bool ps_visit_var(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("VAR", "");
    ps_identifier identifier[8];
    int var_count;
    ps_type_definition *type;
    ps_value *value;
    ps_value_data data;
    ps_symbol *variable;
    EXPECT_TOKEN(PS_TOKEN_VAR);
    READ_NEXT_TOKEN;
    do
    {
        var_count = 0;
        do
        {
            EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
            COPY_IDENTIFIER(identifier[var_count]);
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
        EXPECT_TOKEN(PS_TOKEN_COLON);
        READ_NEXT_TOKEN;
        switch (lexer->current_token.type)
        {
        case PS_TOKEN_BOOLEAN:
            type = ps_system_boolean.value->data.t;
            data.b = (ps_boolean) false;
            break;
        case PS_TOKEN_CHAR:
            type = ps_system_char.value->data.t;
            data.c = '\0';
            break;
        case PS_TOKEN_INTEGER:
            type = ps_system_integer.value->data.t;
            data.i = 0;
            break;
        case PS_TOKEN_UNSIGNED:
            type = ps_system_unsigned.value->data.t;
            data.u = 0;
            break;
        case PS_TOKEN_REAL:
            type = ps_system_real.value->data.t;
            data.r = 0.0;
            break;
        case PS_TOKEN_STRING:
            type = ps_system_string.value->data.t;
            data.s = NULL;
            break;
        // case PS_TOKEN_ARRAY:
        //     type = ps_system_array.value->data.t;
        //     data.s = NULL;
        //     break;
        case PS_TOKEN_IDENTIFIER:
            RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
        default:
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        }
        READ_NEXT_TOKEN;
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
