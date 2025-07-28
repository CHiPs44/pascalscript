/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_system.h"
#include "ps_token.h"
#include "ps_visit.h"

/**
 * Visit
 *      'IF' expression 'THEN' statement [ 'ELSE' statement ] ;
 */
bool ps_visit_if_then_else(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("IF", "");
    ps_value result = {.type = ps_system_boolean.value->data.t, .data.b = false};

    READ_NEXT_TOKEN;
    if (!ps_visit_expression(interpreter, mode, &result))
        TRACE_ERROR("TEST");
    if (result.type != ps_system_boolean.value->data.t)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
    EXPECT_TOKEN(PS_TOKEN_THEN);
    READ_NEXT_TOKEN;
    if (!ps_visit_statement(interpreter, mode && result.data.b))
        TRACE_ERROR("THEN");
    if (lexer->current_token.type == PS_TOKEN_ELSE)
    {
        READ_NEXT_TOKEN;
        if (!ps_visit_statement(interpreter, mode && !result.data.b))
            TRACE_ERROR("ELSE");
    }

    VISIT_END("OK");
}

/**
 * Visit
 *      repeat_statement = 'REPEAT' statement_list [ ';' ] 'UNTIL' expression ;
 */
bool ps_visit_repeat_until(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("REPEAT_UNTIL", "");
    ps_value result = {.type = ps_system_boolean.value->data.t, .data.b = false};
    uint16_t line = 0;
    uint8_t column = 0;

    ps_lexer_get_cursor(lexer, &line, &column);
    READ_NEXT_TOKEN;
    do
    {
        if (!ps_visit_statement_list(interpreter, mode, PS_TOKEN_UNTIL))
            TRACE_ERROR("STATEMENTS");
        // Skip optional ';'
        if (lexer->current_token.type == PS_TOKEN_SEMI_COLON)
            READ_NEXT_TOKEN;
        EXPECT_TOKEN(PS_TOKEN_UNTIL);
        READ_NEXT_TOKEN;
        if (!ps_visit_expression(interpreter, mode, &result))
            TRACE_ERROR("EXPRESSION");
        if (result.type != ps_system_boolean.value->data.t)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
        if (mode != MODE_EXEC || result.data.b)
            break;
        if (!ps_lexer_set_cursor(lexer, line, column))
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE); // TODO better error code
        READ_NEXT_TOKEN;
    } while (true);

    VISIT_END("OK");
}

/**
 * Visit
 *      'WHILE' expression 'DO' statement
 */
bool ps_visit_while_do(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("WHILE_DO", "");
    ps_value result = {.type = ps_system_boolean.value->data.t, .data.b = false};
    uint16_t line = 0;
    uint8_t column = 0;

    // Save "cursor" position
    if (!ps_lexer_get_cursor(lexer, &line, &column))
        TRACE_ERROR("CURSOR!");
    READ_NEXT_TOKEN;
    do
    {
        if (!ps_visit_expression(interpreter, mode, &result))
            TRACE_ERROR("EXPRESSION");
        if (result.type != ps_system_boolean.value->data.t)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
        EXPECT_TOKEN(PS_TOKEN_DO);
        READ_NEXT_TOKEN;
        if (!ps_visit_statement(interpreter, mode && result.data.b))
            TRACE_ERROR("STATEMENT");
        if (mode != MODE_EXEC || !result.data.b)
            break;
        if (!ps_lexer_set_cursor(lexer, line, column))
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE); // TODO better error code
        READ_NEXT_TOKEN;
    } while (true);

    VISIT_END("OK");
}

/**
 * Visit
 *      for_statement = 'FOR' control_variable ':=' expression ( 'TO' |
 * 'DOWNTO' ) expression 'DO' statement ;
 */
bool ps_visit_for_do(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("FOR_DO", "");
    ps_value start = {.type = ps_system_none.value->data.t, .data.v = NULL};
    ps_value finish = {.type = ps_system_none.value->data.t, .data.v = NULL};
    ps_value step = {.type = ps_system_none.value->data.t, .data.v = NULL};
    ps_value result = {.type = ps_system_boolean.value->data.t, .data.b = false};
    ps_identifier identifier;
    ps_symbol *variable;
    uint16_t line = 0;
    uint8_t column = 0;

    // FOR
    READ_NEXT_TOKEN;
    // IDENTIFIER
    EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
    if (mode == MODE_EXEC)
    {
        COPY_IDENTIFIER(identifier);
        variable = ps_interpreter_find_symbol(interpreter, &identifier, true);
        if (variable == NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
        if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
            RETURN_ERROR(PS_ERROR_EXPECTED_VARIABLE);
        start.type = variable->value->type;
        finish.type = variable->value->type;
    }
    // :=
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(PS_TOKEN_ASSIGN);
    // START VALUE
    READ_NEXT_TOKEN;
    if (!ps_visit_expression(interpreter, mode, &start))
        TRACE_ERROR("START");
    // TO | DOWNTO
    if (lexer->current_token.type == PS_TOKEN_TO)
        step.data.i = 1;
    else if (lexer->current_token.type == PS_TOKEN_DOWNTO)
        step.data.i = -1;
    else
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    READ_NEXT_TOKEN;
    // FINISH VALUE
    if (!ps_visit_expression(interpreter, mode, &finish))
        TRACE_ERROR("FINISH");
    // DO
    EXPECT_TOKEN(PS_TOKEN_DO);
    ps_lexer_get_cursor(lexer, &line, &column);
    READ_NEXT_TOKEN;
    if (mode != MODE_EXEC)
    {
        if (!ps_visit_statement_or_compound_statement(interpreter, MODE_SKIP))
            TRACE_ERROR("STATEMENT_OR_COMPOUND");
    }
    else
    {
        // VARIABLE := START
        if (!ps_interpreter_copy_value(interpreter, &start, variable->value))
            TRACE_ERROR("COPY");
        // Loop while variable <= finish for "TO"
        // (or variable >= finish for "DOWNTO")
        do
        {
            if (!ps_function_binary_op(interpreter, variable->value, &finish, &result,
                                       step.data.i > 0 ? PS_TOKEN_LESS_OR_EQUAL : PS_TOKEN_GREATER_OR_EQUAL))
                TRACE_ERROR("BINARY");
            if (result.type != ps_system_boolean.value->data.t)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
            if (!result.data.b)
            {
                // End of loop => skip statement
                if (!ps_visit_statement_or_compound_statement(interpreter, MODE_SKIP))
                    TRACE_ERROR("BODY");
                break;
            }
            if (!ps_visit_statement_or_compound_statement(interpreter, mode))
                TRACE_ERROR("BODY");
            if (!ps_lexer_set_cursor(lexer, line, column))
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE); // TODO better error code
            READ_NEXT_TOKEN;
            // VARIABLE := SUCC/PRED(VARIABLE)
            if (step.data.i > 0)
            {
                if (!ps_function_succ(interpreter, variable->value, variable->value))
                    TRACE_ERROR("STEP/SUCC");
            }
            else
            {
                if (!ps_function_pred(interpreter, variable->value, variable->value))
                    TRACE_ERROR("STEP/PRED");
            }
        } while (true);
    }

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
    // NB: no parameters for now
    EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);

    if (mode == MODE_EXEC)
    {
        executable = calloc(1, sizeof(ps_executable)); // TODO ps_executable_alloc(NULL, NULL, line, column);
        if (executable == NULL)
        {
            interpreter->error = PS_ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
        executable->signature = NULL;
        executable->return_type = NULL;
        executable->line = line;
        executable->column = column;
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

/**
 * Visit statement sequence, stopping at "stop" token (e.g. END, ELSE, UNTIL)
 */
bool ps_visit_statement_list(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_token_type stop)
{
    VISIT_BEGIN("STATEMENT_LIST", "");

    if (lexer->current_token.type == stop)
    {
        READ_NEXT_TOKEN;
    }
    else
    {
        // let's go!
        do
        {
            if (!ps_visit_statement(interpreter, mode))
                TRACE_ERROR("STATEMENT");
            // NB: semi-colon at statement list end is optional
            if (lexer->current_token.type == PS_TOKEN_SEMI_COLON)
            {
                READ_NEXT_TOKEN;
                if (lexer->current_token.type == stop)
                    break;
            }
            else if (lexer->current_token.type == stop)
            {
                break;
            }
            else
            {
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
            }
        } while (true);
    }

    VISIT_END("OK");
}

/**
 * Visit statement or compound statement:
 *      statement_or_compound_statement = statement | compound_statement
 */
bool ps_visit_statement_or_compound_statement(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("STATEMENT_OR_COMPOUND_STATEMENT", "");

    if (lexer->current_token.type == PS_TOKEN_BEGIN)
    {
        if (!ps_visit_compound_statement(interpreter, mode))
            TRACE_ERROR("COMPOUND");
    }
    else
    {
        if (!ps_visit_statement(interpreter, mode))
            TRACE_ERROR("STATEMENT");
    }

    VISIT_END("OK");
}

/**
 * Visit BEGIN
 *         [ STATEMENT ... ] [ ; ]
 *       END
 * NB: ';' or '.' or whatever after END is analyzed in the caller
 */
bool ps_visit_compound_statement(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("COMPOUND_STATEMENT", "");

    EXPECT_TOKEN(PS_TOKEN_BEGIN);
    READ_NEXT_TOKEN;
    if (lexer->current_token.type != PS_TOKEN_END)
    {
        if (!ps_visit_statement_list(interpreter, mode, PS_TOKEN_END))
            TRACE_ERROR("STATEMENTS");
    }
    EXPECT_TOKEN(PS_TOKEN_END);
    READ_NEXT_TOKEN;

    VISIT_END("OK");
}

/**
 * Visit statement
 *      compound_statement      =   'BEGIN' statement_list [ ';' ] 'END' ;
 *      statement               =   assignment_statement
 *                              |   procedure_call
 *                              |   if_statement
 *                              |   repeat_statement
 *                              |   while_statement
 *                              |   for_statement
 *                              ;
 */
bool ps_visit_statement(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("STATEMENT", "");

    switch (lexer->current_token.type)
    {
    case PS_TOKEN_BEGIN:
        if (!ps_visit_compound_statement(interpreter, mode))
            TRACE_ERROR("COMPOUND");
        break;
    case PS_TOKEN_IDENTIFIER:
        if (!ps_visit_assignment_or_procedure_call(interpreter, mode))
            TRACE_ERROR("ASSIGNMENT/PROCEDURE");
        break;
    case PS_TOKEN_IF:
        if (!ps_visit_if_then_else(interpreter, mode))
            TRACE_ERROR("IF");
        break;
    case PS_TOKEN_REPEAT:
        if (!ps_visit_repeat_until(interpreter, mode))
            TRACE_ERROR("REPEAT");
        break;
    case PS_TOKEN_WHILE:
        if (!ps_visit_while_do(interpreter, mode))
            TRACE_ERROR("WHILE");
        break;
    case PS_TOKEN_FOR:
        if (!ps_visit_for_do(interpreter, mode))
            TRACE_ERROR("FOR");
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    }

    VISIT_END("OK");
}

