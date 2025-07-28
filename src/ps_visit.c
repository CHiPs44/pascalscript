/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_functions.h"
#include "ps_parser.h"
#include "ps_procedures.h"
#include "ps_string.h"
#include "ps_system.h"
#include "ps_vm.h"
#include "ps_visit.h"

/**
 * Visit IDENTIFIER := EXPRESSION
 */
bool ps_visit_assignment(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_identifier *identifier)
{
    VISIT_BEGIN("ASSIGNMENT", "");
    ps_symbol *variable;
    ps_value result = {.type = ps_system_none.value->data.t, .data.v = NULL};

    EXPECT_TOKEN(PS_TOKEN_ASSIGN);
    READ_NEXT_TOKEN;

    if (mode == MODE_EXEC)
    {
        variable = ps_interpreter_find_symbol(interpreter, identifier, false);
        if (variable == NULL)
        {
            interpreter->error = PS_ERROR_SYMBOL_NOT_FOUND;
            TRACE_ERROR("VARIABLE1");
        }
        if (variable->kind == PS_SYMBOL_KIND_CONSTANT)
        {
            interpreter->error = PS_ERROR_ASSIGN_TO_CONST;
            TRACE_ERROR("CONST");
        }
        if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
        {
            interpreter->error = PS_ERROR_EXPECTED_VARIABLE;
            TRACE_ERROR("VARIABLE2");
        }
        result.type = variable->value->type;
        if (!ps_visit_expression(interpreter, mode, &result))
            TRACE_ERROR("EXPRESSION1");
        if (interpreter->debug)
            ps_value_debug(stderr, "ASSIGN => ", &result);
        if (!ps_interpreter_copy_value(interpreter, &result, variable->value))
            TRACE_ERROR("COPY");
    }
    else if (!ps_visit_expression(interpreter, MODE_SKIP, &result))
        TRACE_ERROR("EXPRESSION2");

    VISIT_END("OK");
}

/**
 * Visit
 *      write_or_writeln        =   ( 'WRITE' | 'WRITELN' ) [ '(' expression [ ',' expression ]* ')' ] ;
 * Next step:
 *      write_or_writeln        =   ( 'WRITE' | 'WRITELN' ) [ '('
 *                                            expression [ ':' width [ ':' precision ] ]
 *                                      [ ',' expression [ ':' width [ ':' precision ] ] ]*
 *                                    ')' ] ;
 */
bool ps_visit_write_or_writeln(ps_interpreter *interpreter, ps_interpreter_mode mode, bool newline)
{
    VISIT_BEGIN("WRITE_OR_WRITELN", "");
    ps_value result = {.type = ps_system_none.value->data.t, .data.v = NULL};
    bool loop = true;

    // "Write[Ln];" or "Write[Ln] Else|End|Until"?
    if (lexer->current_token.type == PS_TOKEN_SEMI_COLON || lexer->current_token.type == PS_TOKEN_ELSE ||
        lexer->current_token.type == PS_TOKEN_END || lexer->current_token.type == PS_TOKEN_UNTIL)
    {
        if (mode == MODE_EXEC && newline)
            fprintf(stdout, "\n");
        VISIT_END("EMPTY1");
    }
    EXPECT_TOKEN(PS_TOKEN_LEFT_PARENTHESIS);
    READ_NEXT_TOKEN;
    // "Write[Ln]()"?
    if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
    {
        if (mode == MODE_EXEC && newline)
            fprintf(stdout, "\n");
        READ_NEXT_TOKEN;
        loop = false;
    }

    while (loop)
    {
        if (!ps_visit_expression(interpreter, mode, &result))
            TRACE_ERROR("EXPR");
        if (mode == MODE_EXEC)
        {
            if (!ps_procedure_write(interpreter, stdout, &result))
                TRACE_ERROR("WRITE");
        }
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            READ_NEXT_TOKEN;
            continue;
        }
        EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
        READ_NEXT_TOKEN;
        loop = false;
    }

    if (mode == MODE_EXEC && newline)
        fprintf(stdout, "\n");

    VISIT_END("OK");
}

bool ps_visit_procedure_call(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol *executable, uint16_t line,
                             uint8_t column)
{
    VISIT_BEGIN("PROCEDURE_CALL", "");
    bool has_environment = false;

    if (executable == &ps_system_procedure_write || executable == &ps_system_procedure_writeln)
    {
        // Write or Writeln
        if (!ps_visit_write_or_writeln(interpreter, mode, executable == &ps_system_procedure_writeln))
            TRACE_ERROR("WRITE!");
    }
    else if (executable == &ps_system_procedure_randomize)
    {
        // Randomize
        if (mode == MODE_EXEC)
            if (!ps_procedure_randomize(interpreter, NULL))
                TRACE_ERROR("RANDOMIZE!");
    }
    else
    {
        // User defined procedure call
        // Enter environment for procedure call
        if (!ps_interpreter_enter_environment(interpreter, &executable->name))
            RETURN_ERROR(interpreter->error);
        has_environment = true;
        // TODO Parse parameters (needs environment)
        // for now, just check for statement terminators
        ps_token_type token_type = ps_parser_expect_token_types(
            interpreter->parser, 4, (ps_token_type[]){PS_TOKEN_SEMI_COLON, PS_TOKEN_END, PS_TOKEN_ELSE, PS_TOKEN_UNTIL});
        if (token_type == PS_TOKEN_NONE)
        {
            interpreter->error = PS_ERROR_UNEXPECTED_TOKEN;
            goto cleanup;
        }
        // READ_NEXT_TOKEN;
        // Execute procedure
        if (mode == MODE_EXEC)
        {
            fprintf(stderr, "================================================================================\n");
            ps_token_debug(stderr, "CURRENT", &lexer->current_token);
            // Set cursor to the beginning of the procedure body
            if (!ps_lexer_set_cursor(lexer, executable->value->data.x->line, executable->value->data.x->column))
            {
                interpreter->error = PS_ERROR_GENERIC; // TODO better error code
                goto cleanup;
            }
            READ_NEXT_TOKEN;
            fprintf(stderr, "================================================================================\n");
            // Parse procedure body
            if (!ps_visit_block(interpreter, mode))
                goto cleanup;
            // Restore cursor position
            if (!ps_lexer_set_cursor(lexer, line, column))
            {
                interpreter->error = PS_ERROR_GENERIC; // TODO better error code
                goto cleanup;
            }
            READ_NEXT_TOKEN;
        }
        // Exit environment
        if (!ps_interpreter_exit_environment(interpreter))
            RETURN_ERROR(interpreter->error);
    }

    VISIT_END("OK");

cleanup:
    if (has_environment)
        ps_interpreter_exit_environment(interpreter);
    if (interpreter->error == PS_ERROR_NONE)
        interpreter->error = PS_ERROR_GENERIC;
    TRACE_ERROR("CLEANUP");
}

bool ps_visit_assignment_or_procedure_call(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("ASSIGNMENT_OR_PROCEDURE_CALL", "");
    uint16_t line = 0;
    uint8_t column = 0;
    ps_identifier identifier;
    ps_symbol *symbol;

    TRACE_CURSOR;
    // Save "cursor" position
    if (!ps_lexer_get_cursor(lexer, &line, &column))
        RETURN_ERROR(PS_ERROR_GENERIC); // TODO better error code
    COPY_IDENTIFIER(identifier);
    READ_NEXT_TOKEN;
    symbol = ps_interpreter_find_symbol(interpreter, &identifier, false);
    if (symbol == NULL)
        RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
    switch (symbol->kind)
    {
    case PS_SYMBOL_KIND_VARIABLE:
        if (!ps_visit_assignment(interpreter, mode, &identifier))
            TRACE_ERROR("ASSIGN!");
        break;
    case PS_SYMBOL_KIND_CONSTANT:
        RETURN_ERROR(PS_ERROR_ASSIGN_TO_CONST);
    case PS_SYMBOL_KIND_PROCEDURE:
        if (!ps_visit_procedure_call(interpreter, mode, symbol, line, column))
            RETURN_ERROR(interpreter->error);
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
    }

    VISIT_END("OK");
}

/**
 * Visit
 *  PROGRAM IDENTIFIER ';'
 *  BLOCK
 *  '.'
 */
bool ps_visit_start(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("START", "");

    READ_NEXT_TOKEN;
    if (!ps_visit_program(interpreter, mode))
        TRACE_ERROR("PROGRAM");

    VISIT_END("OK");
    return false;
}
