/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_parse_statement.h"
#include "ps_array.h"
#include "ps_functions.h"
#include "ps_parse.h"
#include "ps_procedures.h"
#include "ps_symbol.h"
#include "ps_system.h"
#include "ps_token.h"

/**
 * Parse statement:
 *      'BEGIN' statement_list [ ';' ] 'END'
 *      assignment_statement
 *      procedure_call_statement
 *      if_then_else_statement
 *      repeat_until_statement
 *      while_do_statement
 *      for_to_downto_do_statement
 */
bool ps_parse_statement(ps_compiler *compiler)
{
    PARSE_BEGIN("STATEMENT", "");

    switch (lexer->current_token.type)
    {
    case PS_TOKEN_BEGIN:
        if (!ps_parse_compound_statement(compiler))
            TRACE_ERROR("COMPOUND")
        break;
    case PS_TOKEN_IDENTIFIER:
        if (!ps_parse_assignment_or_procedure_call(compiler))
            TRACE_ERROR("ASSIGNMENT/PROCEDURE")
        break;
    case PS_TOKEN_IF:
        if (!ps_parse_if_then_else(compiler))
            TRACE_ERROR("IF")
        break;
    case PS_TOKEN_REPEAT:
        if (!ps_parse_repeat_until(compiler))
            TRACE_ERROR("REPEAT")
        break;
    case PS_TOKEN_WHILE:
        if (!ps_parse_while_do(compiler))
            TRACE_ERROR("WHILE")
        break;
    case PS_TOKEN_FOR:
        if (!ps_parse_for_do(compiler))
            TRACE_ERROR("FOR")
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }

    PARSE_END("OK")
}

/**
 * Parse compound statement:
 *      'BEGIN'
 *          [ STATEMENT [ ';' STATEMENT ]* ] [ ';' ]
 *      'END'
 * NB: ';' or '.' or whatever after END is analyzed in the caller
 */
bool ps_parse_compound_statement(ps_compiler *compiler)
{
    PARSE_BEGIN("COMPOUND_STATEMENT", "");

    EXPECT_TOKEN(PS_TOKEN_BEGIN);
    READ_NEXT_TOKEN
    if (lexer->current_token.type != PS_TOKEN_END && !ps_parse_statement_list(compiler, PS_TOKEN_END))
        TRACE_ERROR("STATEMENT_LIST")
    EXPECT_TOKEN(PS_TOKEN_END)
    READ_NEXT_TOKEN

    PARSE_END("OK")
}

bool ps_parse_assignment_array(ps_compiler *compiler, ps_symbol *variable)
{
    PARSE_BEGIN("ASSIGNMENT", "ARRAY")

    ps_value result = {.allocated = false, .type = &ps_system_none, .data.v = NULL};
    ps_symbol *item_type = ps_array_get_subrange(variable);
    u_int8_t dimensions = ps_array_get_dimensions(variable);
    if (dimensions > 8)
        RETURN_ERROR(PS_ERROR_TOO_MANY_DIMENSIONS)
    ps_value indexes[8] = {0};
    int dimension = 0;
    // Initialize index types for each dimension
    bool loop = true;
    do
    {
        indexes[dimension].allocated = false;
        indexes[dimension].type = item_type;
        indexes[dimension].data.v = NULL;
        if (!loop)
            break;
        dimension += 1;
        item_type = item_type->value->type->value->data.t->def.a.subrange;
        if (item_type->kind != PS_TYPE_ARRAY)
            loop = false;
    } while (true);

    EXPECT_TOKEN(PS_TOKEN_LEFT_BRACKET)
    READ_NEXT_TOKEN
    do
    {
        // At least one index
        if (!ps_parse_expression(compiler, &indexes[dimension]))
            TRACE_ERROR("INDEX")
        dimension += 1;
        // ',' begins another index
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            // Too many indexes?
            if (dimension == dimensions)
                RETURN_ERROR(PS_ERROR_TOO_MANY_DIMENSIONS)
            READ_NEXT_TOKEN
            continue;
        }
        // ']' ends indexes (and loop)
        if (lexer->current_token.type == PS_TOKEN_RIGHT_BRACKET)
        {
            // Not enough indexes?
            if (dimension != dimensions)
                RETURN_ERROR(PS_ERROR_NOT_ENOUGH_DIMENSIONS)
            READ_NEXT_TOKEN
            break;
        }
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    } while (true);
    // Check for ':='
    EXPECT_TOKEN(PS_TOKEN_ASSIGN)
    READ_NEXT_TOKEN
    // Parse expression for value, expected type is item type
    result.type = item_type;
    if (!ps_parse_expression(compiler, &result))
        TRACE_ERROR("EXPRESSION1")
    // if (mode == MODE_EXEC)
    // {
    //     ps_error error = ps_array_set_value(variable, &indexes, &result, compiler->range_check);
    //     if (error != PS_ERROR_NONE)
    //     {
    //         compiler->error = error;
    //         TRACE_ERROR("ARRAY_ASSIGN")
    //     }
    // }

    PARSE_END("OK")
}

/**
 * Parse assignment:
 *      IDENTIFIER := EXPRESSION
 * Next steps:
 *  Array access:
 *      IDENTIFIER '[' EXPRESSION [ ',' EXPRESSION ]* ']' := EXPRESSION
 *  Pointer dereference:
 *      IDENTIFIER '^' = EXPRESSION
 *      IDENTIFIER '[' EXPRESSION [ ',' EXPRESSION ]* ']' '^' := EXPRESSION
 */
bool ps_parse_assignment(ps_compiler *compiler, ps_symbol *variable)
{
    PARSE_BEGIN("ASSIGNMENT", "")

    ps_value result = {.type = &ps_system_none, .data.v = NULL};

    if (variable->kind == PS_SYMBOL_KIND_CONSTANT)
    {
        compiler->error = PS_ERROR_ASSIGN_TO_CONST;
        ps_interpreter_set_message(compiler, "Constant '%s' cannot be assigned", variable->name);
        TRACE_ERROR("CONSTANT");
    }
    if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
    {
        compiler->error = PS_ERROR_EXPECTED_VARIABLE;
        ps_interpreter_set_message(compiler, "Symbol '%s' is not a variable", variable->name);
        TRACE_ERROR("VARIABLE");
    }
    if (compiler->debug >= DEBUG_VERBOSE)
        fprintf(stderr, "\nINFO\tASSIGNMENT: #1 variable '%s' type is '%s'\n", variable->name,
                ps_type_definition_get_name(variable->value->type->value->data.t));
    if (ps_value_get_type(variable->value) == PS_TYPE_ARRAY)
    {
        // => array[index] := expression
        if (!ps_parse_assignment_array(compiler, variable))
            TRACE_ERROR("ARRAY")
    }
    else
    {
        EXPECT_TOKEN(PS_TOKEN_ASSIGN);
        READ_NEXT_TOKEN
        result.type = variable->value->type;
        if (!ps_parse_expression(compiler, &result))
            TRACE_ERROR("EXPRESSION1");
        if (compiler->debug >= DEBUG_VERBOSE)
            fprintf(stderr, "\nINFO\tASSIGNMENT: #2 variable '%s' type is '%s'\n", variable->name,
                    ps_type_definition_get_name(variable->value->type->value->data.t));
        // if (mode == MODE_EXEC && !ps_interpreter_copy_value(compiler, &result, variable->value))
        //     TRACE_ERROR("COPY");
    }

    PARSE_END("OK")
}

bool ps_parse_read_or_readln(ps_compiler *compiler, bool newline)
{
    (void)newline;
    PARSE_BEGIN("READ_OR_READLN", "")
    RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
}

/**
 * Parse
 *      'WRITE' | 'WRITELN' [ '('
 *          expression [ ':' width [ ':' precision ] ]
 *          [ ',' expression [ ':' width [ ':' precision ] ] ]*
 *      ')' ] ;
 * Next steps:
 *   Write to text file:
 *      'WRITE' | 'WRITELN' [ '('
 *          [ file_variable ',' ]
 *          expression [ ':' width [ ':' precision ] ]
 *          [ ',' expression [ ':' width [ ':' precision ] ] ]*
 *      ')' ] ;
 *   Write to binary file:
 *      'WRITE' '('
 *          file_variable ',' expression
 *      ')' ;
 */
bool ps_parse_write_or_writeln(ps_compiler *compiler, bool newline) // NOSONAR
{
    PARSE_BEGIN("WRITE_OR_WRITELN", "");

    ps_value result = {.type = &ps_system_none, .data.v = NULL};
    bool loop = true;
    int16_t width = 0;
    int16_t precision = 0;

    // "Write[Ln];" or "Write[Ln] Else|End|Until"?
    // (Write without parameters is legal but is a no-op)
    if (PS_TOKEN_NONE != ps_parser_expect_statement_end_token(compiler->parser))
    {
        // if (mode == MODE_EXEC && newline)
        //     fprintf(stdout, "\n");
        PARSE_END("EMPTY1");
    }
    EXPECT_TOKEN(PS_TOKEN_LEFT_PARENTHESIS);
    READ_NEXT_TOKEN
    // "Write[Ln]()"?
    if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
    {
        // if (mode == MODE_EXEC && newline)
        //     fprintf(stdout, "\n");
        READ_NEXT_TOKEN
        loop = false;
    }

    while (loop)
    {
        result.type = &ps_system_none;
        if (compiler->debug >= DEBUG_VERBOSE)
            fprintf(stderr, "\nINFO\tWRITE_OR_WRITELN: expecting expression of type 'ANY'\n");
        if (!ps_parse_expression(compiler, &result))
            TRACE_ERROR("EXPRESSION");
        // retrieve numeric format
        width = 0;
        precision = 0;
        if (lexer->current_token.type == PS_TOKEN_COLON)
        {
            READ_NEXT_TOKEN
            EXPECT_TOKEN(PS_TOKEN_UNSIGNED_VALUE);
            width = (int16_t)(lexer->current_token.value.u);
            READ_NEXT_TOKEN
            if (lexer->current_token.type == PS_TOKEN_COLON)
            {
                READ_NEXT_TOKEN
                EXPECT_TOKEN(PS_TOKEN_UNSIGNED_VALUE);
                precision = (int16_t)(lexer->current_token.value.u);
                READ_NEXT_TOKEN
            }
        }
        // if (mode == MODE_EXEC && !ps_procedure_write(compiler, stdout, &result, width, precision))
        //     TRACE_ERROR(newline ? "WRITELN" : "WRITE");
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            READ_NEXT_TOKEN
            continue;
        }
        EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
        READ_NEXT_TOKEN
        loop = false;
    }

    // if (mode == MODE_EXEC && newline)
    //     fprintf(stdout, "\n");

    PARSE_END("OK")
}

/**
 * Parse assignment or procedure call:
 *  this is determined by the symbol kind:
 *      - variable: assignment
 *      - procedure: procedure call
 */
bool ps_parse_assignment_or_procedure_call(ps_compiler *compiler)
{
    PARSE_BEGIN("ASSIGNMENT_OR_PROCEDURE_CALL", "");
    ps_identifier identifier;
    ps_symbol *symbol;
    ps_identifier result_identifier = "RESULT";

    COPY_IDENTIFIER(identifier)
    READ_NEXT_TOKEN

    // Check if identifier is the current function as defined in its parent environment
    ps_environment *environment = ps_interpreter_get_environment(compiler);
    if (environment == NULL)
        RETURN_ERROR(PS_ERROR_ENVIRONMENT_UNDERFLOW);
    // First, check if this is an assignment to the current function name
    symbol = ps_environment_find_symbol(environment->parent, identifier, true);
    if (symbol != NULL && symbol->kind == PS_SYMBOL_KIND_FUNCTION && strcmp((char *)identifier, environment->name) == 0)
    {
        if (compiler->debug >= DEBUG_VERBOSE)
            fprintf(stderr, "%cINFO\tAssignment to current function '%s' as Result\n", (char *)identifier);
        // Assign to the not so implicit "Result" local variable
        symbol = ps_interpreter_find_symbol(compiler, result_identifier, false);
        if (symbol == NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
    }
    else
    {
        // Normal lookup - can be variable, constant, procedure, or function
        symbol = ps_interpreter_find_symbol(compiler, identifier, false);
    }

    if (symbol == NULL)
        RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);

    switch (symbol->kind)
    {
    case PS_SYMBOL_KIND_VARIABLE:
        if (!ps_parse_assignment(compiler, symbol))
            TRACE_ERROR("ASSIGNMENT")
        break;
    case PS_SYMBOL_KIND_CONSTANT:
        ps_interpreter_set_message(compiler, "Constant '%s' cannot be assigned", symbol->name);
        RETURN_ERROR(PS_ERROR_ASSIGN_TO_CONST)
    case PS_SYMBOL_KIND_PROCEDURE:
        if (!ps_parse_procedure_or_function_call(compiler, symbol, NULL))
            TRACE_ERROR("PROCEDURE_CALL")
        break;
    case PS_SYMBOL_KIND_FUNCTION:
        // Assignment to function name = assignment to Result
        symbol = ps_interpreter_find_symbol(compiler, result_identifier, false);
        if (symbol == NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND)
        if (!ps_parse_assignment(compiler, symbol))
            TRACE_ERROR("ASSIGNMENT")
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }

    PARSE_END("OK")
}

/**
 * Parse
 *      'IF' expression 'THEN' statement [ 'ELSE' statement ]
 */
bool ps_parse_if_then_else(ps_compiler *compiler)
{
    PARSE_BEGIN("IF", "")

    ps_value result = {.type = &ps_system_boolean, .data.b = false};

    READ_NEXT_TOKEN
    if (!ps_parse_expression(compiler, &result))
        TRACE_ERROR("CONDITION")
    if (result.type != &ps_system_boolean)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
    EXPECT_TOKEN(PS_TOKEN_THEN)
    READ_NEXT_TOKEN
    if (!ps_parse_statement(compiler))
        TRACE_ERROR("THEN")
    if (lexer->current_token.type == PS_TOKEN_ELSE)
    {
        READ_NEXT_TOKEN
        if (!ps_parse_statement(compiler))
            TRACE_ERROR("ELSE")
    }

    PARSE_END("OK")
}

/**
 * Parse
 *      'REPEAT' statement_list [ ';' ] 'UNTIL' expression ;
 */
bool ps_parse_repeat_until(ps_compiler *compiler)
{
    PARSE_BEGIN("REPEAT_UNTIL", "");
    ps_value result = {.type = &ps_system_boolean, .data.b = false};
    uint16_t line = 0;
    uint16_t column = 0;

    ps_lexer_get_cursor(lexer, &line, &column);
    READ_NEXT_TOKEN
    do
    {
        if (!ps_parse_statement_list(compiler, PS_TOKEN_UNTIL))
            TRACE_ERROR("STATEMENTS");
        // Skip optional ';'
        if (lexer->current_token.type == PS_TOKEN_SEMI_COLON)
            READ_NEXT_TOKEN
        EXPECT_TOKEN(PS_TOKEN_UNTIL);
        READ_NEXT_TOKEN
        if (!ps_parse_expression(compiler, &result))
            TRACE_ERROR("EXPRESSION");
        if (result.type != &ps_system_boolean)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
        // if (mode != MODE_EXEC || result.data.b)
        //     break;
        // if (!ps_lexer_set_cursor(lexer, line, column))
        //     RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE); // TODO better error code
        READ_NEXT_TOKEN
    } while (true);

    PARSE_END("OK")
}

/**
 * Parse
 *      'WHILE' expression 'DO' statement
 */
bool ps_parse_while_do(ps_compiler *compiler)
{
    PARSE_BEGIN("WHILE_DO", "");
    ps_value result = {.type = &ps_system_boolean, .data.b = false};
    uint16_t line = 0;
    uint16_t column = 0;

    // Save "cursor" position
    if (!ps_lexer_get_cursor(lexer, &line, &column))
        TRACE_ERROR("CURSOR!");
    READ_NEXT_TOKEN
    do
    {
        if (!ps_parse_expression(compiler, &result))
            TRACE_ERROR("EXPRESSION");
        if (result.type != &ps_system_boolean)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
        EXPECT_TOKEN(PS_TOKEN_DO);
        READ_NEXT_TOKEN
        if (!ps_parse_statement(compiler))
            TRACE_ERROR("STATEMENT");
        // if (mode != MODE_EXEC || !result.data.b)
        //     break;
        // if (!ps_lexer_set_cursor(lexer, line, column))
        //     RETURN_ERROR(PS_ERROR_GENERIC) // TODO better error code
        READ_NEXT_TOKEN
    } while (true);

    PARSE_END("OK")
}

/**
 * Parse
 *      'FOR' control_variable ':=' expression ( 'TO' | 'DOWNTO' ) expression 'DO' statement ;
 */
bool ps_parse_for_do(ps_compiler *compiler)
{
    PARSE_BEGIN("FOR_DO", "");

    ps_value start = {.type = &ps_system_none, .data.v = NULL};
    ps_value finish = {.type = &ps_system_none, .data.v = NULL};
    bool downto = false;
    ps_value result = {.type = &ps_system_boolean, .data.b = false};
    ps_identifier identifier = {0};
    ps_symbol *variable = NULL;
    uint16_t line = 0;
    uint16_t column = 0;

    // FOR
    READ_NEXT_TOKEN
    // IDENTIFIER
    EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
    COPY_IDENTIFIER(identifier)
    variable = ps_interpreter_find_symbol(compiler, identifier, true);
    if (variable == NULL)
        RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
    if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
        RETURN_ERROR(PS_ERROR_EXPECTED_VARIABLE);
    if (!ps_value_is_ordinal(variable->value))
        RETURN_ERROR(PS_ERROR_EXPECTED_ORDINAL)
    start.type = variable->value->type;
    finish.type = variable->value->type;
    // :=
    READ_NEXT_TOKEN
    EXPECT_TOKEN(PS_TOKEN_ASSIGN);
    // START VALUE
    READ_NEXT_TOKEN
    if (!ps_parse_expression(compiler, &start))
        TRACE_ERROR("START");
    // TO | DOWNTO
    if (lexer->current_token.type == PS_TOKEN_TO)
        downto = false;
    else if (lexer->current_token.type == PS_TOKEN_DOWNTO)
        downto = true;
    else
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN
    // FINISH VALUE
    if (!ps_parse_expression(compiler, &finish))
        TRACE_ERROR("FINISH");
    // DO
    EXPECT_TOKEN(PS_TOKEN_DO);
    // // Save "cursor"
    // ps_lexer_get_cursor(lexer, &line, &column);
    READ_NEXT_TOKEN
    // if (mode != MODE_EXEC)
    // {
    //     if (!ps_parse_statement_or_compound_statement(compiler_SKIP))
    //         TRACE_ERROR("STATEMENT_OR_COMPOUND")
    // }
    // else
    // {
    // // VARIABLE := START
    // if (!ps_interpreter_copy_value(compiler, &start, variable->value))
    //     TRACE_ERROR("COPY");
    // do
    // {
    //     // Loop while variable <= finish for "TO"
    //     //         or variable >= finish for "DOWNTO"
    //     if (!ps_function_binary_op(compiler, variable->value, &finish, &result,
    //                                downto ? PS_TOKEN_GE : PS_TOKEN_LE))
    //         TRACE_ERROR("BINARY")
    //     if (result.type != &ps_system_boolean)
    //         RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE)
    //     if (!result.data.b)
    //     {
    //         // End of loop => skip statement
    //         if (!ps_parse_statement_or_compound_statement(compiler_SKIP))
    //             TRACE_ERROR("BODY")
    //         break;
    //     }
    //     if (!ps_parse_statement_or_compound_statement(compiler))
    //         TRACE_ERROR("BODY");
    //     // Next iteration
    //     if (!ps_lexer_set_cursor(lexer, line, column))
    //         RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE); // TODO better error code
    //     READ_NEXT_TOKEN
    //     bool range_check = compiler->range_check;
    //     compiler->range_check = false;
    //     compiler->error = downto ? ps_function_pred(compiler, variable->value, variable->value)
    //                                 : ps_function_succ(compiler, variable->value, variable->value);
    //     compiler->range_check = range_check;
    //     if (compiler->error != PS_ERROR_NONE)
    //         TRACE_ERROR(downto ? "STEP/PRED" : "STEP/SUCC")
    //     } while (true);
    // }

    PARSE_END("OK")
}

/**
 * Parse statement sequence, stopping at "stop" token (e.g. END, ELSE, UNTIL)
 */
bool ps_parse_statement_list(ps_compiler *compiler, ps_token_type stop)
{
    PARSE_BEGIN("STATEMENT_LIST", "");

    // Empty block?
    if (lexer->current_token.type == stop)
    {
        READ_NEXT_TOKEN
    }
    else
    {
        // Let's go!
        bool loop = true;
        do
        {
            if (!ps_parse_statement(compiler))
                TRACE_ERROR("STATEMENT");
            // NB: semi-colon at statement list end is optional
            if (lexer->current_token.type == PS_TOKEN_SEMI_COLON)
            {
                READ_NEXT_TOKEN
                if (lexer->current_token.type == stop) // NOSONAR
                    loop = false;
            }
            else if (lexer->current_token.type == stop)
                loop = false;
            else
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        } while (loop);
    }

    PARSE_END("OK")
}

/**
 * Parse statement or compound statement:
 *      statement_or_compound_statement = statement | compound_statement
 */
bool ps_parse_statement_or_compound_statement(ps_compiler *compiler)
{
    PARSE_BEGIN("STATEMENT_OR_COMPOUND_STATEMENT", "");

    if (lexer->current_token.type == PS_TOKEN_BEGIN)
    {
        if (!ps_parse_compound_statement(compiler))
            TRACE_ERROR("COMPOUND");
    }
    else
    {
        if (!ps_parse_statement(compiler))
            TRACE_ERROR("STATEMENT");
    }

    PARSE_END("OK")
}
