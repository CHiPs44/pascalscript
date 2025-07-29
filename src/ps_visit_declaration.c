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
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN;
    }
    program = ps_symbol_alloc(PS_SYMBOL_KIND_PROGRAM, &identifier, NULL);
    if (!ps_interpreter_add_symbol(interpreter, program))
        RETURN_ERROR(interpreter->error);
    if (!ps_interpreter_enter_environment(interpreter, &identifier))
        RETURN_ERROR(interpreter->error);
    if (!ps_visit_block(interpreter, mode))
        TRACE_ERROR("BLOCK");
    // ps_symbol_table_dump(ps_interpreter_get_environment(interpreter)->symbols, "Before EXIT", stderr);
    ps_interpreter_exit_environment(interpreter);
    EXPECT_TOKEN(PS_TOKEN_DOT);
    // NB: text after '.' is not analyzed and has not to be

    VISIT_END("OK");
}

/**
 * Visit block:
 *       [ CONST ... TYPE ... VAR ... PROCEDURE ... FUNCTION ... ]*
 *       COMPOUND_STATEMENT
 * NB: ; or . or whatever after END is analyzed in the caller
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
            ps_symbol_table_dump(ps_interpreter_get_environment(interpreter)->symbols, "PROCEDURE1?", stderr);
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
 * Visit CONST IDENTIFIER = VALUE;
 *             IDENTIFIER = VALUE;
 *             ...
 * Next steps:
 *       IDENTIFIER = IDENTIFIER | VALUE ;
 *       IDENTIFIER = CONSTANT_EXPRESSION ;
 */
bool ps_visit_const(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    ps_identifier identifier;
    ps_type_definition *type;
    ps_value *value;
    ps_value_data data;
    ps_symbol *constant;
    bool negate = false;

    VISIT_BEGIN("CONST", "");

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
        }
        else
        {
            negate = false;
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
                type = ps_system_integer.value->data.t;
                if (lexer->current_token.value.u > PS_INTEGER_MAX)
                    RETURN_ERROR(PS_ERROR_OUT_OF_RANGE);
                data.i = -lexer->current_token.value.u;
            }
            else
            {
                type = ps_system_unsigned.value->data.t;
                data.u = lexer->current_token.value.u;
            }
            break;
        case PS_TOKEN_CHAR_VALUE:
            if (negate)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
            type = ps_system_char.value->data.t;
            data.c = lexer->current_token.value.c;
            break;
        case PS_TOKEN_BOOLEAN_VALUE:
            if (negate)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
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
 * Visit
 *      TYPE IDENTIFIER '=' INTEGER | UNSIGNED | REAL | BOOLEAN | CHAR | STRING ';'
 *           ...
 * Next steps:
 *      TYPE IDENTIFIER = TYPE_DEFINITION ;
 *      TYPE_DEFINITION = SUBRANGE | ENUMERATION | POINTER | RECORD | ARRAY | FILE | STRING ;
 *       SUBRANGE = INTEGER | UNSIGNED | IDENTIFIER '..' INTEGER | UNSIGNED | IDENTIFIER ;
 */
bool ps_visit_type(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("TYPE", "");
    ps_identifier identifier = {0};
    ps_type_definition *type = NULL;
    ps_value *value = NULL;
    ps_value_data data = {0};
    ps_symbol *symbol = NULL;
    ps_unsigned len = 0;

    EXPECT_TOKEN(PS_TOKEN_TYPE);
    READ_NEXT_TOKEN;
    do
    {
        EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
        COPY_IDENTIFIER(identifier);
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(PS_TOKEN_EQUAL);
        READ_NEXT_TOKEN;
        switch (lexer->current_token.type)
        {
        case PS_TOKEN_INTEGER:
            type = ps_system_integer.value->data.t;
            break;
        case PS_TOKEN_UNSIGNED:
            type = ps_system_unsigned.value->data.t;
            break;
        case PS_TOKEN_REAL:
            type = ps_system_real.value->data.t;
            break;
        case PS_TOKEN_BOOLEAN:
            type = ps_system_boolean.value->data.t;
            break;
        case PS_TOKEN_CHAR:
            type = ps_system_char.value->data.t;
            break;
        case PS_TOKEN_STRING:
            type = ps_system_string.value->data.t;
            // if (lexer->current_token.type == PS_TOKEN_LEFT_BRACKET)
            // {
            //     READ_NEXT_TOKEN;
            //     if (lexer->current_token.type == PS_TOKEN_UNSIGNED_VALUE)
            //     {
            //         len = lexer->current_token.value.u;
            //     }
            //     else if (lexer->current_token.type == PS_TOKEN_IDENTIFIER)
            //     {
            //         symbol = ps_interpreter_find_symbol(interpreter, &lexer->current_token.value.identifier);
            //         if (symbol == NULL)
            //             RETURN_ERROR(PS_ERROR_UNKOWN_IDENTIFIER);
            //         if (symbol->kind != PS_SYMBOL_KIND_CONSTANT ||
            //             symbol->value->type != ps_system_unsigned.value->data.t)
            //             RETURN_ERROR(PS_ERROR_EXPECTED_UNSIGNED);
            //         len = (ps_unsigned)lexer->current_token.value.i;
            //     }
            //     else
            //         RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
            //     if (len < 1 || len > PS_STRING_MAX_LEN)
            //         RETURN_ERROR(PS_ERROR_EXPECTED_STRING_LENGTH);
            // }
            break;
        // case PS_TOKEN_ARRAY:
        //     type = ps_system_array.value->data.t; // TODO: parse array definition
        //     data.s = NULL; // TODO: allocate array
        //     break;
        case PS_TOKEN_IDENTIFIER:
            RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
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
            symbol = ps_symbol_alloc(PS_SYMBOL_KIND_TYPE_DEFINITION, &identifier, value);
            if (symbol == NULL)
                RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
            if (!ps_interpreter_add_symbol(interpreter, symbol))
                RETURN_ERROR(interpreter->error);
        }
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

/**
 * Visit
 *      PROCEDURE IDENTIFIER ;
 * Next steps:
 *  - allow procedure block with empty body:
 *      PROCEDURE IDENTIFIER ;
 *      BEGIN
 *      END ;
 *  - allow procedure block (constants, variables, body):
 *      PROCEDURE IDENTIFIER
 *      [ CONST ... TYPE ... VAR ... ]*
 *      BEGIN
 *          COMPOUND_STATEMENT [ ; ]
 *      END ;
 *  - allow procedure parameters
 */
bool ps_visit_procedure_or_function(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol_kind kind)
{
    VISIT_BEGIN("PROCEDURE_OR_FUNCTION", "");
    ps_identifier identifier;
    ps_symbol *callable = NULL;
    ps_value *value = NULL;
    ps_executable *executable = NULL;
    uint16_t line = 0;
    uint8_t column = 0;
    bool has_environment = false;

    if (kind != PS_SYMBOL_KIND_PROCEDURE && kind != PS_SYMBOL_KIND_FUNCTION)
    {
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    }

    if (kind == PS_SYMBOL_KIND_FUNCTION)
    {
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
    }

    READ_NEXT_TOKEN;
    EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
    COPY_IDENTIFIER(identifier);
    callable = ps_interpreter_find_symbol(interpreter, &identifier, true);
    if (callable != NULL)
        RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS);

    TRACE_CURSOR;
    if (!ps_lexer_get_cursor(lexer, &line, &column))
    {
        interpreter->error = PS_ERROR_GENERIC; // TODO better error code
        goto cleanup;
    }
    READ_NEXT_TOKEN;
    // NB: no parameters nor parenthesis for now
    EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);

    if (mode == MODE_EXEC)
    {
        executable = ps_executable_alloc(NULL, ps_system_none.value->type, line, column);
        if (executable == NULL)
        {
            interpreter->error = PS_ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
        callable = ps_symbol_alloc(PS_SYMBOL_KIND_PROCEDURE, &identifier, NULL);
        if (callable == NULL)
        {
            interpreter->error = PS_ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
        callable->kind = kind;
        value = ps_value_alloc(ps_system_procedure.value->data.t, (ps_value_data){.x = executable});
        if (value == NULL)
        {
            interpreter->error = PS_ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
        callable->value = value;
        if (!ps_interpreter_add_symbol(interpreter, callable))
        {
            goto cleanup;
        }
        if (!ps_interpreter_enter_environment(interpreter, &identifier))
        {
            goto cleanup;
        }
        has_environment = true;
    }
    // Skip block
    fprintf(stderr, "================================================================================\n");
    ps_token_debug(stderr, "CURRENT", &lexer->current_token);
    ps_executable_debug(stderr, "EXECUTABLE", executable);
    READ_NEXT_TOKEN;
    fprintf(stderr, "================================================================================\n");
    if (!ps_visit_block(interpreter, MODE_SKIP))
    {
        goto cleanup;
    }
    if (mode == MODE_EXEC)
    {
        if (!ps_interpreter_exit_environment(interpreter))
        {
            has_environment = false;
            goto cleanup;
        }
    }
    EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
    READ_NEXT_TOKEN;

    VISIT_END("OK");

cleanup:
    if (has_environment)
        ps_interpreter_exit_environment(interpreter);
    if (callable != NULL)
        ps_symbol_free(callable);
    if (value != NULL)
        ps_value_free(value);
    if (executable != NULL)
        free(executable); // TODO ps_executable_free(executable);
    if (has_environment)
        ps_interpreter_exit_environment(interpreter);
    if (interpreter->error == PS_ERROR_NONE)
        interpreter->error = PS_ERROR_GENERIC;
    TRACE_ERROR("CLEANUP");
}
