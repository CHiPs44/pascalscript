/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_functions.h"
#include "ps_system.h"
#include "ps_parser.h"

// clang-format off
#define USE_LEXER                       ps_lexer *lexer = ps_parser_get_lexer(interpreter->parser)
#define SET_VISITOR(__VISITOR__)        static char *visitor = __VISITOR__
#define READ_NEXT_TOKEN                 { \
                                            if (!ps_lexer_read_next_token(lexer)) \
                                                return false; \
                                            if (interpreter->debug) \
                                            { \
                                                fprintf(stderr, "%cTOKEN\t%-32s %-8s ", exec ? '*' : ' ', "", ""); \
                                                ps_token_dump(&lexer->current_token); \
                                            } \
                                        }
#define EXPECT_TOKEN(__PS_TOKEN_TYPE__) if (!ps_parser_expect_token_type(interpreter->parser, __PS_TOKEN_TYPE__)) return false
#define RETURN_ERROR(__PS_ERROR__)      { \
                                            interpreter->error = __PS_ERROR__; \
                                            if (interpreter->debug) { \
                                                fprintf(stderr, "%cRETURN\t%-32s %-8d ", exec ? '*' : ' ', visitor, __PS_ERROR__); \
                                                ps_token_dump(&lexer->current_token); \
                                            } \
                                            return false; \
                                        }
#define COPY_IDENTIFIER(__IDENTIFIER__) strncpy(__IDENTIFIER__, lexer->current_token.value.identifier, PS_IDENTIFIER_LEN)
#define TRACE_BEGIN(__PLUS__)           if (interpreter->debug) { \
                                            fprintf(stderr, "%cBEGIN\t%-32s %-8s ", exec ? '*' : ' ', visitor, __PLUS__); \
                                            ps_token_dump(&lexer->current_token); \
                                        }
#define TRACE_END(__PLUS__)             if (interpreter->debug) { \
                                            fprintf(stderr, "%cEND\t%-32s %-8s ", exec ? '*' : ' ', visitor, __PLUS__); \
                                            ps_token_dump(&lexer->current_token); \
                                        }
#define TRACE_ERROR(__PLUS__)           { \
                                            if (interpreter->debug) { \
                                                fprintf(stderr, "%cERROR\t%-32s %-8s ", exec ? '*' : ' ', visitor, __PLUS__); \
                                                ps_token_dump(&lexer->current_token); \
                                            } \
                                            return false; \
                                        }
// clang-format on

bool ps_visit_expression(ps_interpreter *interpreter, bool exec, ps_value *result);

bool ps_visit_function_call(ps_interpreter *interpreter, bool exec, ps_symbol *symbol, ps_value *result)
{
    USE_LEXER;
    SET_VISITOR("FUNCTION_CALL");
    TRACE_BEGIN("");
    ps_value value;

    if (lexer->current_token.type == PS_TOKEN_LEFT_PARENTHESIS)
    {
        READ_NEXT_TOKEN;
        if (!ps_visit_expression(interpreter, exec, &value))
            TRACE_ERROR("");
        EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
        READ_NEXT_TOKEN;
    }
    else
    {
        value.type = NULL;
        value.data.i = 0;
    }
    if (exec)
    {
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
        // if (!ps_function_exec(interpreter, symbol, &value, result))
        //     TRACE_ERROR("");
    }

    TRACE_END("OK");
    return true;
}

/**
 * Visit
 *  factor  =   '(' , expression , ')'
 *          |   identifier
 *          |   char_value | integer_value | unsigned_value | real_value | boolean_value
 *          |   [ '+' | '-' | 'NOT' ] , factor
 *          ;
 */
bool ps_visit_factor(ps_interpreter *interpreter, bool exec, ps_value *result)
{
    USE_LEXER;
    SET_VISITOR("FACTOR");
    TRACE_BEGIN("");
    ps_value factor = {.type = ps_system_none.value->data.t, .data.v = NULL};
    ps_identifier identifier;
    ps_symbol *symbol;
    ps_token_type unary_operator;

    switch (lexer->current_token.type)
    {
    case PS_TOKEN_LEFT_PARENTHESIS:
        READ_NEXT_TOKEN;
        if (!ps_visit_expression(interpreter, exec, result))
            TRACE_ERROR("");
        EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_IDENTIFIER:
        // variable, constant, function
        if (exec)
        {
            COPY_IDENTIFIER(identifier);
            symbol = ps_symbol_table_get(interpreter->parser->symbols, &identifier);
            if (symbol == NULL)
                RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_FOUND);
            switch (symbol->kind)
            {
            case PS_SYMBOL_KIND_AUTO:
            case PS_SYMBOL_KIND_CONSTANT:
            case PS_SYMBOL_KIND_VARIABLE:
                if (interpreter->debug)
                {
                    ps_symbol_debug(stderr, "SYMBOL\t", symbol);
                    ps_value_debug(stderr, "RESULT\t", result);
                }
                if (!ps_function_copy_value(interpreter, symbol->value, result))
                    TRACE_ERROR("COPY");
                break;
            case PS_SYMBOL_KIND_FUNCTION:
                if (!ps_visit_function_call(interpreter, exec, symbol, result))
                    TRACE_ERROR("FUNCTION");
            default:
                break;
            }
        }
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_CHAR_VALUE:
        if (exec)
        {
            result->type = ps_system_char.value->data.t;
            result->data.c = lexer->current_token.value.c;
        }
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_INTEGER_VALUE:
        if (exec)
        {
            result->type = ps_system_integer.value->data.t;
            result->data.i = lexer->current_token.value.i;
        }
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_UNSIGNED_VALUE:
        if (exec)
        {
            result->type = ps_system_unsigned.value->data.t;
            result->data.u = lexer->current_token.value.u;
        }
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_REAL_VALUE:
        if (exec)
        {
            result->type = ps_system_real.value->data.t;
            result->data.r = lexer->current_token.value.r;
        }
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_BOOLEAN_VALUE:
        if (exec)
        {
            result->type = ps_system_boolean.value->data.t;
            result->data.b = lexer->current_token.value.b;
        }
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_PLUS:
        READ_NEXT_TOKEN;
        if (!ps_visit_factor(interpreter, exec, result))
            TRACE_ERROR("");
        break;
    case PS_TOKEN_MINUS:
    case PS_TOKEN_NOT:
        unary_operator = lexer->current_token.type;
        READ_NEXT_TOKEN;
        if (!ps_visit_factor(interpreter, exec, &factor))
            TRACE_ERROR("");
        if (exec)
        {
            if (!ps_function_unary_op(interpreter, &factor, result, unary_operator))
                TRACE_ERROR("");
        }
        break;
    case PS_TOKEN_STRING_VALUE:
    case PS_TOKEN_NIL:
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        TRACE_ERROR("");
    default:
        interpreter->error = PS_PARSER_ERROR_UNEXPECTED_TOKEN;
        TRACE_ERROR("");
    }

    TRACE_END("OK");
    return true;
}

/**
 * Visit
 *      term                    =   factor [ multiplicative_operator , factor ]* ;
 *      multiplicative_operator =   '*' | '/' | 'DIV' | 'MOD' | 'AND' | 'SHL' | 'SHR' | 'AS'
 */
bool ps_visit_term(ps_interpreter *interpreter, bool exec, ps_value *result)
{
    static ps_token_type multiplicative_operators[] = {
        PS_TOKEN_STAR, PS_TOKEN_SLASH, PS_TOKEN_DIV, PS_TOKEN_MOD, PS_TOKEN_AND,
        // PS_TOKEN_SHL, PS_TOKEN_SHR,
    };
    USE_LEXER;
    SET_VISITOR("TERM");
    TRACE_BEGIN("");

    ps_value left = {.type = ps_system_none.value->data.t, .data.v = NULL},
             right = {.type = ps_system_none.value->data.t, .data.v = NULL};
    ps_token_type multiplicative_operator = PS_TOKEN_NONE;
    if (!ps_visit_factor(interpreter, exec, &left))
        TRACE_ERROR("");
    do
    {
        multiplicative_operator = ps_parser_expect_token_types(
            interpreter->parser,
            sizeof(multiplicative_operators) / sizeof(ps_token_type),
            multiplicative_operators);
        if (multiplicative_operator == PS_TOKEN_NONE)
        {
            if (exec)
            {
                result->type = left.type;
                result->data = left.data;
            }
            TRACE_END("1");
            return true;
        }
        READ_NEXT_TOKEN;
        if (!ps_visit_factor(interpreter, exec, &right))
            TRACE_ERROR("");
        if (!ps_function_binary_op(interpreter, &left, &right, result, multiplicative_operator))
            TRACE_ERROR("");
        if (exec)
        {
            left.type = result->type;
            left.data = result->data;
        }
    } while (true);

    TRACE_END("2");
    return true;
}

/**
 * Visit
 *      simple_expression       =   term [ additive_operator , term ]* ;
 *      additive_operator       =   '+' | '-' | 'OR' | 'XOR' ;
 */
bool ps_visit_simple_expression(ps_interpreter *interpreter, bool exec, ps_value *result)
{
    static ps_token_type additive_operators[] = {PS_TOKEN_PLUS, PS_TOKEN_MINUS, PS_TOKEN_OR, PS_TOKEN_XOR};
    USE_LEXER;
    SET_VISITOR("SIMPLE_EXPRESSION");
    TRACE_BEGIN("");
    ps_value left = {.type = ps_system_none.value->data.t, .data.v = NULL},
             right = {.type = ps_system_none.value->data.t, .data.v = NULL};
    ps_token_type additive_operator = PS_TOKEN_NONE;
    if (!ps_visit_term(interpreter, exec, &left))
        TRACE_ERROR("");
    do
    {
        additive_operator = ps_parser_expect_token_types(
            interpreter->parser,
            sizeof(additive_operators) / sizeof(ps_token_type),
            additive_operators);
        if (additive_operator == PS_TOKEN_NONE)
        {
            if (exec)
            {
                result->type = left.type;
                result->data = left.data;
            }
            TRACE_END("1");
            return true;
        }
        READ_NEXT_TOKEN;
        if (!ps_visit_factor(interpreter, exec, &right))
            TRACE_ERROR("");
        if (exec)
        {
            if (!ps_function_binary_op(interpreter, &left, &right, result, additive_operator))
                TRACE_ERROR("");
            left.type = result->type;
            left.data = result->data;
        }
    } while (true);
    TRACE_END("2");
    return true;
}

/**
 * Visit
 *  expression              =   simple_expression [ relational_operator , simple_expression ] ;
 *  relational_operator     =   '<' | '<=' | '>' | '>=' | '=' | '<>' | 'IN' | 'IS' ;
 */
bool ps_visit_expression(ps_interpreter *interpreter, bool exec, ps_value *result)
{
    static ps_token_type relational_operators[] = {
        PS_TOKEN_LESS_THAN, PS_TOKEN_LESS_OR_EQUAL,
        PS_TOKEN_GREATER_THAN, PS_TOKEN_GREATER_OR_EQUAL,
        PS_TOKEN_EQUAL, PS_TOKEN_NOT_EQUAL, PS_TOKEN_IN, // PS_TOKEN_IS,
    };
    USE_LEXER;
    SET_VISITOR("EXPRESSION");
    TRACE_BEGIN("");
    ps_value left = {.type = ps_system_none.value->data.t, .data.v = NULL},
             right = {.type = ps_system_none.value->data.t, .data.v = NULL};
    ps_token_type relational_operator = PS_TOKEN_NONE;
    if (!ps_visit_simple_expression(interpreter, exec, &left))
        TRACE_ERROR("");
    relational_operator = ps_parser_expect_token_types(
        interpreter->parser,
        sizeof(relational_operators) / sizeof(ps_token_type),
        relational_operators);
    if (relational_operator == PS_TOKEN_NONE)
    {
        if (exec)
        {
            result->type = left.type;
            result->data = left.data;
        }
        TRACE_END("2");
        return true;
    }
    READ_NEXT_TOKEN;
    if (!ps_visit_simple_expression(interpreter, exec, &right))
        TRACE_ERROR("");
    if (exec)
    {
        result->type = ps_system_boolean.value->data.t;
        if (!ps_function_binary_op(interpreter, &left, &right, result, relational_operator))
            TRACE_ERROR("");
    }
    TRACE_END("2");
    return true;
}

/**
 * PROGRAM IDENTIFIER ;
 */
bool ps_visit_program(ps_interpreter *interpreter, bool exec)
{
    USE_LEXER;
    SET_VISITOR("PROGRAM");
    TRACE_BEGIN("");
    ps_identifier identifier;
    EXPECT_TOKEN(PS_TOKEN_PROGRAM);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
    COPY_IDENTIFIER(identifier);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
    READ_NEXT_TOKEN;
    if (exec)
    {
        ps_symbol *program = ps_symbol_init(
            PS_SYMBOL_SCOPE_GLOBAL,
            PS_SYMBOL_KIND_PROGRAM,
            &identifier,
            NULL);
        if (ps_symbol_table_add(interpreter->parser->symbols, program) == NULL)
            RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_ADDED);
    }
    TRACE_END("OK");
    return true;
}

/**
 * Visit CONST IDENTIFIER = VALUE;
 *             IDENTIFIER = VALUE;
 *             ...
 * Next steps:
 *       IDENTIFIER = IDENTIFIER | VALUE ;
 *       IDENTIFIER = CONSTANT_EXPRESSION ;
 */
bool ps_visit_const(ps_interpreter *interpreter, bool exec)
{
    USE_LEXER;
    SET_VISITOR("CONST");
    TRACE_BEGIN("");
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
        }
        else
        {
            negate = false;
        }
        switch (lexer->current_token.type)
        {
        case PS_TOKEN_IDENTIFIER:
            constant = ps_symbol_table_get(interpreter->parser->symbols, &lexer->current_token.value.identifier);
            if (constant == NULL)
                RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_FOUND);
            if (constant->kind != PS_SYMBOL_KIND_CONSTANT)
                RETURN_ERROR(PS_RUNTIME_ERROR_EXPECTED_CONSTANT);
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
                data.i = -lexer->current_token.value.i;
            }
            else
            {
                type = ps_system_unsigned.value->data.t;
                data.u = lexer->current_token.value.u;
            }
            break;
        case PS_TOKEN_CHAR_VALUE:
            if (negate)
                RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN)
            type = ps_system_char.value->data.t;
            data.c = lexer->current_token.value.c;
            break;
        case PS_TOKEN_BOOLEAN_VALUE:
            if (negate)
                RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN)
            type = ps_system_boolean.value->data.t;
            data.b = lexer->current_token.value.b;
            break;
        // Not yet!
        // case PS_TOKEN_STRING_VALUE:
        //     type = ps_symbol_string.value->data.t;
        //     strncpy(data.s + 1, lexer->current_token.value.s, PS_STRING_MAX_LEN);
        //     break;
        default:
            RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN);
        }
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN;
        if (exec)
        {
            value = ps_value_init(type, data);
            if (value == NULL)
                RETURN_ERROR(PS_RUNTIME_ERROR_OUT_OF_MEMORY);
            constant = ps_symbol_init(
                PS_SYMBOL_SCOPE_GLOBAL,
                PS_SYMBOL_KIND_CONSTANT,
                &identifier,
                value);
            if (constant == NULL)
                RETURN_ERROR(PS_RUNTIME_ERROR_OUT_OF_MEMORY);
            if (ps_symbol_table_add(interpreter->parser->symbols, constant) == NULL)
                RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_ADDED);
        }
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);

    TRACE_END("OK");
    return true;
}

/**
 * Visit    VAR IDENTIFIER : TYPE;
 *              IDENTIFIER : TYPE;
 *          ...
 * Next step: allow identifier list with commas
 *              IDENTIFIER, IDENTIFIER, ... : TYPE ;
 */
bool ps_visit_var(ps_interpreter *interpreter, bool exec)
{
    USE_LEXER;
    SET_VISITOR("VAR");
    TRACE_BEGIN("");
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
                    RETURN_ERROR(PS_PARSER_ERROR_TOO_MANY_VARIABLES);
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
        case PS_TOKEN_IDENTIFIER:
            RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
        default:
            RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN);
        }
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN;
        if (exec)
        {
            for (int i = 0; i <= var_count; i++)
            {
                value = ps_value_init(type, data);
                variable = ps_symbol_init(
                    PS_SYMBOL_SCOPE_GLOBAL,
                    PS_SYMBOL_KIND_VARIABLE,
                    &identifier[i],
                    value);
                if (variable == NULL)
                    RETURN_ERROR(PS_RUNTIME_ERROR_OUT_OF_MEMORY);
                if (ps_symbol_table_add(interpreter->parser->symbols, variable) == NULL)
                    RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_ADDED);
            }
        }
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);
    TRACE_END("OK");
    return true;
}

/**
 * Visit IDENTIFIER := EXPRESSION
 */
bool ps_visit_assignment(ps_interpreter *interpreter, bool exec, ps_identifier *identifier)
{
    USE_LEXER;
    SET_VISITOR("ASSIGNMENT");
    TRACE_BEGIN("");
    ps_symbol *variable;
    ps_value result = {.type = ps_system_none.value->data.t, .data.v = NULL};

    EXPECT_TOKEN(PS_TOKEN_ASSIGN);
    READ_NEXT_TOKEN;

    if (exec)
    {
        variable = ps_symbol_table_get(interpreter->parser->symbols, identifier);
        if (variable == NULL)
        {
            interpreter->error = PS_RUNTIME_ERROR_SYMBOL_NOT_FOUND;
            TRACE_ERROR("");
        }
        if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
        {
            interpreter->error = PS_RUNTIME_ERROR_EXPECTED_VARIABLE;
            TRACE_ERROR("");
        }
        result.type = variable->value->type;
        if (!ps_visit_expression(interpreter, exec, &result))
            TRACE_ERROR("");
        if (interpreter->debug)
            ps_value_debug(stderr, "ASSIGN => ", &result);
        if (!ps_function_copy_value(interpreter, &result, variable->value))
            TRACE_ERROR("");
        // variable->value->data = result.data;
    }
    else if (!ps_visit_expression(interpreter, false, &result))
        TRACE_ERROR("");

    TRACE_END("OK");
    return true;
}

/**
 * Visit
 *      write_or_writeln        =   ( 'WRITE' | 'WRITELN' ) '(' expression , expression ]* ')' ;
 * Next step:
 *      write_or_writeln        =   WRITE | WRITELN '('
 *                                            expression [ ':' width [ ':' precision ] ]
 *                                      [ ',' expression [ ':' width [ ':' precision ] ] ]* ) ;
 */
bool ps_visit_write_or_writeln(ps_interpreter *interpreter, bool exec, bool newline)
{
    USE_LEXER;
    SET_VISITOR("WRITE_OR_WRITELN");
    TRACE_BEGIN("");
    ps_value result = {.type = ps_system_none.value->data.t, .data.v = NULL};
    bool loop = true;

    // "Write[Ln];" or "Write[Ln] Else|End|Until"?
    if (lexer->current_token.type == PS_TOKEN_SEMI_COLON ||
        lexer->current_token.type == PS_TOKEN_ELSE ||
        lexer->current_token.type == PS_TOKEN_END ||
        lexer->current_token.type == PS_TOKEN_UNTIL)
    {
        if (exec && newline)
            fprintf(stdout, "\n");
        TRACE_END("EMPTY1");
        return true;
    }
    EXPECT_TOKEN(PS_TOKEN_LEFT_PARENTHESIS);
    READ_NEXT_TOKEN;
    // "Write[Ln]()"?
    if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
    {
        if (exec && newline)
            fprintf(stdout, "\n");
        READ_NEXT_TOKEN;
        loop = false;
    }

    while (loop)
    {
        if (!ps_visit_expression(interpreter, exec, &result))
            TRACE_ERROR("EXPR");
        if (exec)
        {
            if (!ps_function_write_text(interpreter, stdout, &result))
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

    if (exec && newline)
        fprintf(stdout, "\n");

    TRACE_END("OK");
    return true;
}

bool ps_visit_assignment_or_procedure_call(ps_interpreter *interpreter, bool exec)
{
    USE_LEXER;
    SET_VISITOR("ASSIGNMENT_OR_PROCEDURE_CALL");
    TRACE_BEGIN("");
    ps_identifier identifier;
    ps_symbol *symbol;

    COPY_IDENTIFIER(identifier);
    READ_NEXT_TOKEN;
    symbol = ps_symbol_table_get(interpreter->parser->symbols, &identifier);
    if (symbol == NULL)
        RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_FOUND);
    switch (symbol->kind)
    {
    case PS_SYMBOL_KIND_VARIABLE:
        if (!ps_visit_assignment(interpreter, exec, &identifier))
            TRACE_ERROR("ASSIGN!");
        break;
    case PS_SYMBOL_KIND_CONSTANT:
        RETURN_ERROR(PS_RUNTIME_ERROR_ASSIGN_TO_CONST);
    case PS_SYMBOL_KIND_PROCEDURE:
        if (symbol == &ps_system_procedure_write || symbol == &ps_system_procedure_writeln)
        {
            if (!ps_visit_write_or_writeln(interpreter, exec, symbol == &ps_system_procedure_writeln))
                TRACE_ERROR("WRITE!");
        }
        else if (symbol == &ps_system_procedure_randomize)
        {
            if (!ps_function_randomize(interpreter))
                TRACE_ERROR("RANDOMIZE!");
        }
        else
        {
            RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
        }
        break;
    default:
        RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN);
    }

    TRACE_END("OK");
    return true;
}

/* forward declarations */
bool ps_visit_statement_list(ps_interpreter *interpreter, bool exec, ps_token_type stop);
bool ps_visit_statement_or_compound_statement(ps_interpreter *interpreter, bool exec);
bool ps_visit_statement(ps_interpreter *interpreter, bool exec);

/**
 * Visit BEGIN
 *         [ STATEMENT ... ] [ ; ]
 *       END
 * NB: ; or . or whatever after END is analyzed in the caller
 */
bool ps_visit_compound_statement(ps_interpreter *interpreter, bool exec)
{
    USE_LEXER;
    SET_VISITOR("COMPOUND_STATEMENT");
    TRACE_BEGIN("");

    EXPECT_TOKEN(PS_TOKEN_BEGIN);
    READ_NEXT_TOKEN;
    if (lexer->current_token.type != PS_TOKEN_END)
    {
        if (!ps_visit_statement_list(interpreter, exec, PS_TOKEN_END))
            TRACE_ERROR("");
    }
    EXPECT_TOKEN(PS_TOKEN_END);
    READ_NEXT_TOKEN;

    TRACE_END("OK");
    return true;
}

/**
 * Visit
 *      if_statement = 'IF' expression 'THEN' statement [ 'ELSE' statement ] ;
 */
bool ps_visit_if_then_else(ps_interpreter *interpreter, bool exec)
{
    USE_LEXER;
    SET_VISITOR("IF_THEN_ELSE");
    ps_value result = {.type = ps_system_boolean.value->data.t, .data.b = false};
    TRACE_BEGIN("");

    EXPECT_TOKEN(PS_TOKEN_IF);
    READ_NEXT_TOKEN;
    if (!ps_visit_expression(interpreter, exec, &result))
        TRACE_ERROR("TEST");
    if (result.type != ps_system_boolean.value->data.t)
        RETURN_ERROR(PS_RUNTIME_ERROR_UNEXPECTED_TYPE);
    EXPECT_TOKEN(PS_TOKEN_THEN);
    READ_NEXT_TOKEN;
    if (!ps_visit_statement(interpreter, exec && result.data.b))
        TRACE_ERROR("");
    if (lexer->current_token.type == PS_TOKEN_ELSE)
    {
        READ_NEXT_TOKEN;
        if (!ps_visit_statement(interpreter, exec && !result.data.b))
            TRACE_ERROR("");
    }

    TRACE_END("OK");
    return true;
}

/**
 * Visit
 *      repeat_statement = 'REPEAT' statement_list [ ';' ] 'UNTIL' expression ;
 */
bool ps_visit_repeat_until(ps_interpreter *interpreter, bool exec)
{
    // interpreter->trace = interpreter->debug = true;
    USE_LEXER;
    SET_VISITOR("REPEAT_UNTIL");
    ps_value result = {.type = ps_system_boolean.value->data.t, .data.b = false};
    uint16_t line = 0;
    uint16_t column = 0;
    TRACE_BEGIN("");

    // Save "cursor" position
    line = lexer->buffer->current_line;
    column = lexer->buffer->current_column;
    EXPECT_TOKEN(PS_TOKEN_REPEAT);
    READ_NEXT_TOKEN;
    do
    {
        if (!ps_visit_statement_list(interpreter, exec, PS_TOKEN_UNTIL))
            TRACE_ERROR("");
        // Skip optional ';'
        if (lexer->current_token.type == PS_TOKEN_SEMI_COLON)
            READ_NEXT_TOKEN;
        EXPECT_TOKEN(PS_TOKEN_UNTIL);
        READ_NEXT_TOKEN;
        if (!ps_visit_expression(interpreter, exec, &result))
            TRACE_ERROR("");
        if (result.type != ps_system_boolean.value->data.t)
            RETURN_ERROR(PS_RUNTIME_ERROR_UNEXPECTED_TYPE);
        if (!exec || result.data.b)
            break;
        // Restore "cursor" position
        lexer->buffer->current_line = line;
        lexer->buffer->current_column = column;
        // Reset lexer to a known state
        lexer->buffer->current_char = '\0';
        lexer->buffer->next_char = '\0';
        if (!ps_buffer_read_next_char(lexer->buffer))
            RETURN_ERROR(PS_RUNTIME_ERROR_UNEXPECTED_TYPE); // TODO better error code
        READ_NEXT_TOKEN;
    } while (true);

    TRACE_END("OK");
    return true;
}

/**
 * Visit
 *      while_statement = 'WHILE' expression 'DO' statement ;
 */
bool ps_visit_while_do(ps_interpreter *interpreter, bool exec)
{
    USE_LEXER;
    SET_VISITOR("WHILE_DO");
    ps_value result = {.type = ps_system_boolean.value->data.t, .data.b = false};
    uint16_t line = 0;
    uint16_t column = 0;
    TRACE_BEGIN("");

    // Save "cursor" position
    line = lexer->buffer->current_line;
    column = lexer->buffer->current_column;
    EXPECT_TOKEN(PS_TOKEN_WHILE);
    READ_NEXT_TOKEN;
    do
    {
        if (!ps_visit_expression(interpreter, exec, &result))
            TRACE_ERROR("");
        if (result.type != ps_system_boolean.value->data.t)
            RETURN_ERROR(PS_RUNTIME_ERROR_UNEXPECTED_TYPE);
        EXPECT_TOKEN(PS_TOKEN_DO);
        READ_NEXT_TOKEN;
        if (!ps_visit_statement(interpreter, exec && result.data.b))
            TRACE_ERROR("");
        if (!exec || !result.data.b)
            break;
        // Restore "cursor" position
        lexer->buffer->current_line = line;
        lexer->buffer->current_column = column;
        // Set lexer to a known state
        lexer->buffer->current_char = '\0';
        lexer->buffer->next_char = '\0';
        if (!ps_buffer_read_next_char(lexer->buffer))
            RETURN_ERROR(PS_RUNTIME_ERROR_UNEXPECTED_TYPE); // TODO better error code
        READ_NEXT_TOKEN;
    } while (true);

    TRACE_END("OK");
    return true;
}

/**
 * Visit
 *      for_statement = 'FOR' control_variable ':=' expression ( 'TO' | 'DOWNTO' ) expression 'DO' statement ;
 */
bool ps_visit_for_do(ps_interpreter *interpreter, bool exec)
{
    USE_LEXER;
    SET_VISITOR("FOR_DO");
    ps_value start = {.type = ps_system_integer.value->data.t, .data.v = NULL};
    ps_value finish = {.type = ps_system_integer.value->data.t, .data.v = NULL};
    ps_value step = {.type = ps_system_integer.value->data.t, .data.v = NULL};
    ps_value result = {.type = ps_system_boolean.value->data.t, .data.b = false};
    ps_identifier identifier;
    ps_symbol *variable;
    uint16_t line = 0;
    uint16_t column = 0;
    TRACE_BEGIN("");

    // FOR
    EXPECT_TOKEN(PS_TOKEN_FOR);
    READ_NEXT_TOKEN;
    // IDENTIFIER
    EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
    COPY_IDENTIFIER(identifier);
    variable = ps_symbol_table_get(interpreter->parser->symbols, &identifier);
    if (variable == NULL)
        RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_FOUND);
    if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
        RETURN_ERROR(PS_RUNTIME_ERROR_EXPECTED_VARIABLE);
    READ_NEXT_TOKEN;
    // :=
    EXPECT_TOKEN(PS_TOKEN_ASSIGN);
    READ_NEXT_TOKEN;
    // START VALUE
    if (!ps_visit_expression(interpreter, exec, &start))
        TRACE_ERROR("");
    // TO | DOWNTO
    if (lexer->current_token.type == PS_TOKEN_TO)
        step.data.i = 1;
    else if (lexer->current_token.type == PS_TOKEN_DOWNTO)
        step.data.i = -1;
    else
        RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN);
    READ_NEXT_TOKEN;
    // FINISH VALUE
    if (!ps_visit_expression(interpreter, exec, &finish))
        TRACE_ERROR("");
    // DO
    EXPECT_TOKEN(PS_TOKEN_DO);
    // Save "cursor" position
    line = lexer->buffer->current_line;
    column = lexer->buffer->current_column;
    READ_NEXT_TOKEN;
    if (!exec)
    {
        if (!ps_visit_statement_or_compound_statement(interpreter, false))
            TRACE_ERROR("");
    }
    else
    {
        // VARIABLE := START
        if (!ps_function_copy_value(interpreter, &start, variable->value))
            TRACE_ERROR("COPY");
        // Loop while variable <= finish for TO (or variable >= finish for DOWNTO)
        do
        {
            if (!ps_function_binary_op(
                    interpreter, variable->value, &finish, &result,
                    step.data.i > 0 ? PS_TOKEN_LESS_OR_EQUAL : PS_TOKEN_GREATER_OR_EQUAL))
                TRACE_ERROR("");
            if (result.type != ps_system_boolean.value->data.t)
                RETURN_ERROR(PS_RUNTIME_ERROR_UNEXPECTED_TYPE);
            if (!result.data.b)
            {
                // End of loop => skip statement
                if (!ps_visit_statement_or_compound_statement(interpreter, false))
                    TRACE_ERROR("");
                break;
            }
            if (!ps_visit_statement_or_compound_statement(interpreter, true))
                TRACE_ERROR("");
            // Restore "cursor" position
            lexer->buffer->current_line = line;
            lexer->buffer->current_column = column;
            // Set lexer to a known state
            lexer->buffer->current_char = '\0';
            lexer->buffer->next_char = '\0';
            if (!ps_buffer_read_next_char(lexer->buffer))
                RETURN_ERROR(PS_LEXER_ERROR_UNEXPECTED_EOF); // TODO better error code?
            READ_NEXT_TOKEN;
            // VARIABLE := VARIABLE + STEP
            if (!ps_function_binary_op(interpreter, variable->value, &step, variable->value, PS_TOKEN_PLUS))
                TRACE_ERROR("");
        } while (true);
    }

    TRACE_END("OK");
    return true;
}

/**
 * Visit statement
 *      compound_statement   = 'BEGIN' statement_list [ ';' ] 'END' ;
 *      statement_list       = statement [ ';' ]* ;
 *      statement            = assignment_statement | procedure_call | if_statement | repeat_statement | while_statement | for_statement ;
 *      assignment_statement = ( variable_reference | function_identifier | 'RESULT' ) ':=' expression ;
 *      variable_reference   = identifier ;
 *      function_identifier  = identifier ;
 *      procedure_call       = procedure_identifier [ '(' expression , expression ]* ')' ] ;
 *      procedure_identifier = identifier ;
 *      if_statement         = 'IF' expression 'THEN' statement [ 'ELSE' statement ] ;
 *      repeat_statement     = 'REPEAT' statement_list [ ';' ] 'UNTIL' expression ;
 *      while_statement      = 'WHILE' expression 'DO' statement ;
 *      for_statement        = 'FOR' control_variable ':=' expression ( 'TO' | 'DOWNTO' ) expression 'DO' statement ;
 *      control_variable     = identifier ;
 */
bool ps_visit_statement(ps_interpreter *interpreter, bool exec)
{
    USE_LEXER;
    SET_VISITOR("STATEMENT");
    TRACE_BEGIN("");

    switch (lexer->current_token.type)
    {
    case PS_TOKEN_BEGIN:
        if (!ps_visit_compound_statement(interpreter, exec))
            TRACE_ERROR("");
        break;
    case PS_TOKEN_IDENTIFIER:
        if (!ps_visit_assignment_or_procedure_call(interpreter, exec))
            TRACE_ERROR("");
        break;
    case PS_TOKEN_IF:
        if (!ps_visit_if_then_else(interpreter, exec))
            TRACE_ERROR("");
        break;
    case PS_TOKEN_REPEAT:
        if (!ps_visit_repeat_until(interpreter, exec))
            TRACE_ERROR("");
        break;
    case PS_TOKEN_WHILE:
        if (!ps_visit_while_do(interpreter, exec))
            TRACE_ERROR("");
        break;
    case PS_TOKEN_FOR:
        if (!ps_visit_for_do(interpreter, exec))
            TRACE_ERROR("");
        break;
    default:
        RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN);
    }

    TRACE_END("OK");
    return true;
}

/**
 * Visit statement sequence
 */
bool ps_visit_statement_list(ps_interpreter *interpreter, bool exec, ps_token_type stop)
{
    USE_LEXER;
    SET_VISITOR("STATEMENT_LIST");
    TRACE_BEGIN("");

    if (lexer->current_token.type == stop)
    {
        READ_NEXT_TOKEN;
    }
    else
    {
        // let's go!
        bool loop = true;
        do
        {
            if (!ps_visit_statement(interpreter, exec))
                TRACE_ERROR("");
            if (lexer->current_token.type == PS_TOKEN_SEMI_COLON)
            {
                READ_NEXT_TOKEN;
                if (lexer->current_token.type == stop)
                {
                    loop = false;
                }
            }
            else if (lexer->current_token.type == stop)
            {
                loop = false;
            }
            else
            {
                RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN);
            }
        } while (loop);
    }
    TRACE_END("OK");
    return true;
}

bool ps_visit_statement_or_compound_statement(ps_interpreter *interpreter, bool exec)
{
    USE_LEXER;
    SET_VISITOR("STATEMENT_OR_COMPOUND_STATEMENT");
    TRACE_BEGIN("");
    if (lexer->current_token.type == PS_TOKEN_BEGIN)
    {
        if (!ps_visit_compound_statement(interpreter, exec))
            TRACE_ERROR("");
    }
    else
    {
        if (!ps_visit_statement(interpreter, exec))
            TRACE_ERROR("");
    }
    TRACE_END("OK");
    return true;
}

/**
 * Visit [ CONST ... TYPE ... VAR ... ]*
 *       COMPOUND_STATEMENT
 * NB: ; or . or whatever after END is analyzed in the caller
 */
bool ps_visit_block(ps_interpreter *interpreter, bool exec)
{
    USE_LEXER;
    SET_VISITOR("BLOCK");
    TRACE_BEGIN("");

    do
    {
        if (lexer->current_token.type == PS_TOKEN_CONST && !ps_visit_const(interpreter, exec))
            TRACE_ERROR("");
        // if (lexer->current_token.type == PS_TOKEN_TYPE && !ps_visit_type(interpreter, exec))
        //     TRACE_ERROR("");
        if (lexer->current_token.type == PS_TOKEN_VAR && !ps_visit_var(interpreter, exec))
            TRACE_ERROR("");
    } while (lexer->current_token.type != PS_TOKEN_BEGIN);

    if (!ps_visit_compound_statement(interpreter, exec))
        TRACE_ERROR("");

    TRACE_END("OK");
    return true;
}

/**
 * Visit PROGRAM...
 *       BLOCK
 *       .
 */
bool ps_visit_start(ps_interpreter *interpreter, bool exec)
{
    USE_LEXER;
    SET_VISITOR("START");
    TRACE_BEGIN("");
    READ_NEXT_TOKEN;
    if (!ps_visit_program(interpreter, exec))
        TRACE_ERROR("");
    if (!ps_visit_block(interpreter, exec))
        TRACE_ERROR("");
    EXPECT_TOKEN(PS_TOKEN_DOT);
    // NB: source code after '.' is not analyzed and has not to be
    TRACE_END("OK");
    return true;
}
