/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_functions.h"
#include "ps_procedures.h"
#include "ps_symbol.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_visit.h"

/**
 * Visit statement:
 *      'BEGIN' statement_list [ ';' ] 'END'
 *      assignment_statement
 *      procedure_call_statement
 *      if_then_else_statement
 *      repeat_until_statement
 *      while_do_statement
 *      for_to_downto_do_statement
 */
bool ps_visit_statement(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("STATEMENT", "");

    switch (lexer->current_token.type)
    {
    case PS_TOKEN_BEGIN:
        if (!ps_visit_compound_statement(interpreter, mode))
            TRACE_ERROR("COMPOUND")
        break;
    case PS_TOKEN_IDENTIFIER:
        if (!ps_visit_assignment_or_procedure_call(interpreter, mode))
            TRACE_ERROR("ASSIGNMENT/PROCEDURE")
        break;
    case PS_TOKEN_IF:
        if (!ps_visit_if_then_else(interpreter, mode))
            TRACE_ERROR("IF")
        break;
    case PS_TOKEN_REPEAT:
        if (!ps_visit_repeat_until(interpreter, mode))
            TRACE_ERROR("REPEAT")
        break;
    case PS_TOKEN_WHILE:
        if (!ps_visit_while_do(interpreter, mode))
            TRACE_ERROR("WHILE")
        break;
    case PS_TOKEN_FOR:
        if (!ps_visit_for_do(interpreter, mode))
            TRACE_ERROR("FOR")
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }

    VISIT_END("OK")
}

/**
 * Visit compound statement:
 *      'BEGIN'
 *          [ STATEMENT [ ';' STATEMENT ]* ] [ ';' ]
 *      'END'
 * NB: ';' or '.' or whatever after END is analyzed in the caller
 */
bool ps_visit_compound_statement(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("COMPOUND_STATEMENT", "");

    EXPECT_TOKEN(PS_TOKEN_BEGIN);
    READ_NEXT_TOKEN
    if (lexer->current_token.type == PS_TOKEN_SEMI_COLON)
        READ_NEXT_TOKEN
    if (lexer->current_token.type != PS_TOKEN_END && !ps_visit_statement_list(interpreter, mode, PS_TOKEN_END))
        TRACE_ERROR("STATEMENTS")
    EXPECT_TOKEN(PS_TOKEN_END)
    READ_NEXT_TOKEN

    VISIT_END("OK")
}

/**
 * Visit assignment:
 *      IDENTIFIER := EXPRESSION
 * Next steps:
 *  Array access:
 *      IDENTIFIER '[' EXPRESSION [ ',' EXPRESSION ]* ']' := EXPRESSION
 *  Pointer dereference:
 *      IDENTIFIER '^' = EXPRESSION
 *      IDENTIFIER '[' EXPRESSION [ ',' EXPRESSION ]* ']' '^' := EXPRESSION
 */
bool ps_visit_assignment(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol *variable)
{
    VISIT_BEGIN("ASSIGNMENT", "");

    ps_value result = {.type = &ps_system_none, .data.v = NULL};

    EXPECT_TOKEN(PS_TOKEN_ASSIGN);
    READ_NEXT_TOKEN

    if (variable->kind == PS_SYMBOL_KIND_CONSTANT)
    {
        interpreter->error = PS_ERROR_ASSIGN_TO_CONST;
        ps_interpreter_set_message(interpreter, "Constant '%s' cannot be assigned", variable->name);
        TRACE_ERROR("CONST");
    }
    if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
    {
        interpreter->error = PS_ERROR_EXPECTED_VARIABLE;
        ps_interpreter_set_message(interpreter, "Symbol '%s' is not a variable", variable->name);
        TRACE_ERROR("VARIABLE2");
    }
    if (interpreter->debug >= DEBUG_VERBOSE)
        fprintf(stderr, "\n%cINFO\tASSIGNMENT: #1 variable '%s' type is '%s'\n", mode == MODE_EXEC ? '*' : ' ',
                variable->name, ps_type_definition_get_name(variable->value->type->value->data.t));
    result.type = variable->value->type;
    if (!ps_visit_expression(interpreter, mode, &result))
        TRACE_ERROR("EXPRESSION1");
    if (interpreter->debug >= DEBUG_VERBOSE)
        fprintf(stderr, "\n%cINFO\tASSIGNMENT: #2 variable '%s' type is '%s'\n", mode == MODE_EXEC ? '*' : ' ',
                variable->name, ps_type_definition_get_name(variable->value->type->value->data.t));
    if (mode == MODE_EXEC && !ps_interpreter_copy_value(interpreter, &result, variable->value))
        TRACE_ERROR("COPY");

    VISIT_END("OK")
}

bool ps_visit_read_or_readln(ps_interpreter *interpreter, ps_interpreter_mode mode, bool newline)
{
    (void)newline;
    VISIT_BEGIN("READ_OR_READLN", "");
    RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
}

/**
 * Visit
 *      'WRITE' | 'WRITELN' ) [ '('
 *          expression [ ':' width [ ':' precision ] ]
 *          [ ',' expression [ ':' width [ ':' precision ] ] ]*
 *      ')' ] ;
 * Next steps:
 *      'WRITE' | 'WRITELN' ) [ '('
 *          [ file_variable ',' ]
 *          expression [ ':' width [ ':' precision ] ]
 *          [ ',' expression [ ':' width [ ':' precision ] ] ]*
 *      ')' ] ;
 */
bool ps_visit_write_or_writeln(ps_interpreter *interpreter, ps_interpreter_mode mode, bool newline)
{
    VISIT_BEGIN("WRITE_OR_WRITELN", "");

    ps_value result = {.type = &ps_system_none, .data.v = NULL};
    bool loop = true;
    int16_t width = 0;
    int16_t precision = 0;

    // "Write[Ln];" or "Write[Ln] Else|End|Until"?
    // (Write without parameters is legal but is a no-op)
    if (PS_TOKEN_NONE != ps_parser_expect_statement_end_token(interpreter->parser))
    {
        if (mode == MODE_EXEC && newline)
            fprintf(stdout, "\n");
        VISIT_END("EMPTY1");
    }
    EXPECT_TOKEN(PS_TOKEN_LEFT_PARENTHESIS);
    READ_NEXT_TOKEN
    // "Write[Ln]()"?
    if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
    {
        if (mode == MODE_EXEC && newline)
            fprintf(stdout, "\n");
        READ_NEXT_TOKEN
        loop = false;
    }

    while (loop)
    {
        result.type = &ps_system_none;
        if (interpreter->debug >= DEBUG_VERBOSE)
            fprintf(stderr, "\n%cINFO\tWRITE_OR_WRITELN: expecting expression of type 'ANY'\n",
                    mode == MODE_EXEC ? '*' : ' ');
        if (!ps_visit_expression(interpreter, mode, &result))
            TRACE_ERROR("EXPRESSION");
        // retrieve numeric format
        width = 0;
        precision = 0;
        if (lexer->current_token.type == PS_TOKEN_COLON)
        {
            READ_NEXT_TOKEN
            EXPECT_TOKEN(PS_TOKEN_INTEGER_VALUE);
            width = (int16_t)(lexer->current_token.value.i);
            READ_NEXT_TOKEN
            if (lexer->current_token.type == PS_TOKEN_COLON)
            {
                READ_NEXT_TOKEN
                EXPECT_TOKEN(PS_TOKEN_INTEGER_VALUE);
                precision = (int16_t)(lexer->current_token.value.i);
                READ_NEXT_TOKEN
            }
        }
        if (mode == MODE_EXEC && !ps_procedure_write(interpreter, stdout, &result, width, precision))
            TRACE_ERROR(newline ? "WRITELN" : "WRITE");
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            READ_NEXT_TOKEN
            continue;
        }
        EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
        READ_NEXT_TOKEN
        loop = false;
    }

    if (mode == MODE_EXEC && newline)
        fprintf(stdout, "\n");

    VISIT_END("OK")
}

/**
 * Visit assignment or procedure call:
 *  this is determined by the symbol kind:
 *      - variable: assignment
 *      - procedure: procedure call
 */
bool ps_visit_assignment_or_procedure_call(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("ASSIGNMENT_OR_PROCEDURE_CALL", "");
    ps_identifier identifier;
    ps_symbol *symbol;
    ps_identifier result_identifier = "RESULT";

    COPY_IDENTIFIER(identifier)
    READ_NEXT_TOKEN

    // Check if identifier is the current function as defined in its parent environment
    ps_environment *environment = ps_interpreter_get_environment(interpreter);
    if (environment == NULL)
        RETURN_ERROR(PS_ERROR_ENVIRONMENT_UNDERFLOW);

    // First, check if this is an assignment to the current function name
    symbol = ps_environment_find_symbol(environment->parent, &identifier, true);
    if (symbol != NULL && symbol->kind == PS_SYMBOL_KIND_FUNCTION && strcmp((char *)identifier, environment->name) == 0)
    {
        if (interpreter->debug >= DEBUG_VERBOSE)
            fprintf(stderr, "%cINFO\tAssignment to current function '%s' as Result\n", mode == MODE_EXEC ? '*' : ' ',
                    (char *)identifier);
        // Assign to the not so implicit "Result" local variable
        symbol = ps_interpreter_find_symbol(interpreter, &result_identifier, false);
        if (symbol == NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
    }
    else
    {
        // Normal lookup - can be variable, constant, procedure, or function
        symbol = ps_interpreter_find_symbol(interpreter, &identifier, false);
    }

    if (symbol == NULL)
        RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);

    switch (symbol->kind)
    {
    case PS_SYMBOL_KIND_VARIABLE:
        if (!ps_visit_assignment(interpreter, mode, symbol))
            TRACE_ERROR("ASSIGNMENT")
        break;
    case PS_SYMBOL_KIND_CONSTANT:
        ps_interpreter_set_message(interpreter, "Constant '%s' cannot be assigned", symbol->name);
        RETURN_ERROR(PS_ERROR_ASSIGN_TO_CONST)
    case PS_SYMBOL_KIND_PROCEDURE:
        if (!ps_visit_procedure_or_function_call(interpreter, mode, symbol, NULL))
            TRACE_ERROR("PROCEDURE_CALL")
        break;
    case PS_SYMBOL_KIND_FUNCTION:
        // Assignment to function name = assignment to Result
        symbol = ps_interpreter_find_symbol(interpreter, &result_identifier, false);
        if (symbol == NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND)
        if (!ps_visit_assignment(interpreter, mode, symbol))
            TRACE_ERROR("ASSIGNMENT")
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }

    VISIT_END("OK")
}

/**
 * Visit
 *      'IF' expression 'THEN' statement [ 'ELSE' statement ]
 */
bool ps_visit_if_then_else(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("IF", "")

    ps_value result = {.type = &ps_system_boolean, .data.b = false};

    READ_NEXT_TOKEN
    if (!ps_visit_expression(interpreter, mode, &result))
        TRACE_ERROR("CONDITION")
    if (result.type != &ps_system_boolean)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
    EXPECT_TOKEN(PS_TOKEN_THEN)
    READ_NEXT_TOKEN
    if (!ps_visit_statement(interpreter, mode == MODE_EXEC && result.data.b ? MODE_EXEC : MODE_SKIP))
        TRACE_ERROR("THEN")
    if (lexer->current_token.type == PS_TOKEN_ELSE)
    {
        READ_NEXT_TOKEN
        if (!ps_visit_statement(interpreter, mode == MODE_EXEC && !result.data.b ? MODE_EXEC : MODE_SKIP))
            TRACE_ERROR("ELSE")
    }

    VISIT_END("OK")
}

/**
 * Visit
 *      'REPEAT' statement_list [ ';' ] 'UNTIL' expression ;
 */
bool ps_visit_repeat_until(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("REPEAT_UNTIL", "");
    ps_value result = {.type = &ps_system_boolean, .data.b = false};
    uint16_t line = 0;
    uint16_t column = 0;

    ps_lexer_get_cursor(lexer, &line, &column);
    READ_NEXT_TOKEN
    do
    {
        if (!ps_visit_statement_list(interpreter, mode, PS_TOKEN_UNTIL))
            TRACE_ERROR("STATEMENTS");
        // Skip optional ';'
        if (lexer->current_token.type == PS_TOKEN_SEMI_COLON)
            READ_NEXT_TOKEN
        EXPECT_TOKEN(PS_TOKEN_UNTIL);
        READ_NEXT_TOKEN
        if (!ps_visit_expression(interpreter, mode, &result))
            TRACE_ERROR("EXPRESSION");
        if (result.type != &ps_system_boolean)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
        if (mode != MODE_EXEC || result.data.b)
            break;
        if (!ps_lexer_set_cursor(lexer, line, column))
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE); // TODO better error code
        READ_NEXT_TOKEN
    } while (true);

    VISIT_END("OK")
}

/**
 * Visit
 *      'WHILE' expression 'DO' statement
 */
bool ps_visit_while_do(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("WHILE_DO", "");
    ps_value result = {.type = &ps_system_boolean, .data.b = false};
    uint16_t line = 0;
    uint16_t column = 0;

    // Save "cursor" position
    if (!ps_lexer_get_cursor(lexer, &line, &column))
        TRACE_ERROR("CURSOR!");
    READ_NEXT_TOKEN
    do
    {
        if (!ps_visit_expression(interpreter, mode, &result))
            TRACE_ERROR("EXPRESSION");
        if (result.type != &ps_system_boolean)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
        EXPECT_TOKEN(PS_TOKEN_DO);
        READ_NEXT_TOKEN
        if (!ps_visit_statement(interpreter, result.data.b ? mode : MODE_SKIP))
            TRACE_ERROR("STATEMENT");
        if (mode != MODE_EXEC || !result.data.b)
            break;
        if (!ps_lexer_set_cursor(lexer, line, column))
            RETURN_ERROR(PS_ERROR_GENERIC) // TODO better error code
        READ_NEXT_TOKEN
    } while (true);

    VISIT_END("OK")
}

/**
 * Visit
 *      'FOR' control_variable ':=' expression ( 'TO' |
 * 'DOWNTO' ) expression 'DO' statement ;
 */
bool ps_visit_for_do(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    VISIT_BEGIN("FOR_DO", "");

    ps_value start = {.type = &ps_system_none, .data.v = NULL};
    ps_value finish = {.type = &ps_system_none, .data.v = NULL};
    ps_value step = {.type = &ps_system_none, .data.v = NULL};
    ps_value result = {.type = &ps_system_boolean, .data.b = false};
    ps_identifier identifier = {0};
    ps_symbol *variable = NULL;
    uint16_t line = 0;
    uint16_t column = 0;

    // FOR
    READ_NEXT_TOKEN
    // IDENTIFIER
    EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
    if (mode == MODE_EXEC)
    {
        COPY_IDENTIFIER(identifier)
        variable = ps_interpreter_find_symbol(interpreter, &identifier, true);
        if (variable == NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
        if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
            RETURN_ERROR(PS_ERROR_EXPECTED_VARIABLE);
        start.type = variable->value->type;
        finish.type = variable->value->type;
    }
    // :=
    READ_NEXT_TOKEN
    EXPECT_TOKEN(PS_TOKEN_ASSIGN);
    // START VALUE
    READ_NEXT_TOKEN
    if (!ps_visit_expression(interpreter, mode, &start))
        TRACE_ERROR("START");
    // TO | DOWNTO
    if (lexer->current_token.type == PS_TOKEN_TO)
        step.data.i = 1;
    else if (lexer->current_token.type == PS_TOKEN_DOWNTO)
        step.data.i = -1;
    else
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN
    // FINISH VALUE
    if (!ps_visit_expression(interpreter, mode, &finish))
        TRACE_ERROR("FINISH");
    // DO
    EXPECT_TOKEN(PS_TOKEN_DO);
    ps_lexer_get_cursor(lexer, &line, &column);
    READ_NEXT_TOKEN
    if (mode != MODE_EXEC)
    {
        if (!ps_visit_statement_or_compound_statement(interpreter, MODE_SKIP))
            TRACE_ERROR("STATEMENT_OR_COMPOUND")
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
                                       step.data.i > 0 ? PS_TOKEN_LE : PS_TOKEN_GE))
                TRACE_ERROR("BINARY")
            if (result.type != &ps_system_boolean)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE)
            if (!result.data.b)
            {
                // End of loop => skip statement
                if (!ps_visit_statement_or_compound_statement(interpreter, MODE_SKIP))
                    TRACE_ERROR("BODY")
                break;
            }
            if (!ps_visit_statement_or_compound_statement(interpreter, mode))
                TRACE_ERROR("BODY");
            if (!ps_lexer_set_cursor(lexer, line, column))
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE); // TODO better error code
            READ_NEXT_TOKEN
            // compute VARIABLE := SUCC/PRED(VARIABLE)
            if (step.data.i > 0)
            {
                interpreter->error = ps_function_succ(interpreter, variable->value, variable->value);
                if (interpreter->error != PS_ERROR_NONE)
                    TRACE_ERROR("STEP/SUCC")
            }
            else
            {
                interpreter->error = ps_function_pred(interpreter, variable->value, variable->value);
                if (interpreter->error != PS_ERROR_NONE)
                    TRACE_ERROR("STEP/PRED")
            }
        } while (true);
    }

    VISIT_END("OK")
}

/**
 * Visit statement sequence, stopping at "stop" token (e.g. END, ELSE, UNTIL)
 */
bool ps_visit_statement_list(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_token_type stop)
{
    VISIT_BEGIN("STATEMENT_LIST", "");

    if (lexer->current_token.type == stop)
    {
        READ_NEXT_TOKEN
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
                READ_NEXT_TOKEN
                if (lexer->current_token.type == stop)
                    break;
            }
            else if (lexer->current_token.type == stop)
            {
                break;
            }
            else
            {
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
            }
        } while (true);
    }

    VISIT_END("OK")
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

    VISIT_END("OK")
}
