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
#include "ps_visit.h"

/**
 *  This is the entry point for visiting all expressions.
 */
bool ps_visit_expression(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
{
    return ps_visit_or_expression(interpreter, mode, result);
}

/**
 * Visit
 *      or_expression = and_expression { ( 'OR' | 'XOR' ) and_expression }
 * Goal:
 *      make A < 1 OR A > 10 OR A = 5 OR ... work without parenthesis
 */
bool ps_visit_or_expression(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
{
    VISIT_BEGIN("OR_EXPRESSION", "");

    static ps_token_type or_operators[] = {PS_TOKEN_OR, PS_TOKEN_XOR};
    ps_value left = {.type = result->type, .data.v = NULL};
    ps_value right = {.type = result->type, .data.v = NULL};
    ps_token_type or_operator = PS_TOKEN_NONE;

    if (!ps_visit_and_expression(interpreter, mode, &left))
        TRACE_ERROR("AND");
    do
    {
        or_operator = ps_parser_expect_token_types(interpreter->parser, sizeof(or_operators) / sizeof(ps_token_type),
                                                   or_operators);
        if (or_operator == PS_TOKEN_NONE)
        {
            if (mode == MODE_EXEC)
            {
                result->type = left.type;
                result->data = left.data;
            }
            VISIT_END("LEFT");
        }
        READ_NEXT_TOKEN;
        if (!ps_visit_and_expression(interpreter, mode, &right))
            TRACE_ERROR("AND2");
        if (mode == MODE_EXEC)
        {
            if (!ps_function_binary_op(interpreter, &left, &right, result, or_operator))
                TRACE_ERROR("BINARY_OP");
            left.type = result->type;
            left.data = result->data;
        }
    } while (true);

    VISIT_END("RIGHT");
}

/**
 * Visit and expression:
 *      relational_expression { 'AND' relational_expression }
 * Goal:
 *      make A < 1 AND A > 10 AND A = 5 AND ... work without parenthesis
 */
bool ps_visit_and_expression(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
{
    VISIT_BEGIN("AND_EXPRESSION", "");

    static ps_token_type and_operators[] = {PS_TOKEN_AND};

    ps_value left = {.type = result->type, .data.v = NULL};
    ps_value right = {.type = result->type, .data.v = NULL};
    ps_token_type and_operator = PS_TOKEN_NONE;

    if (!ps_visit_relational_expression(interpreter, mode, &left))
        TRACE_ERROR("RELATIONAL1");
    do
    {
        and_operator = ps_parser_expect_token_types(interpreter->parser, sizeof(and_operators) / sizeof(ps_token_type),
                                                    and_operators);
        if (and_operator == PS_TOKEN_NONE)
        {
            if (mode == MODE_EXEC)
            {
                result->type = left.type;
                result->data = left.data;
            }
            VISIT_END("AND1");
        }
        READ_NEXT_TOKEN;
        if (!ps_visit_relational_expression(interpreter, mode, &right))
            TRACE_ERROR("RELATIONAL2");
        if (mode == MODE_EXEC)
        {
            if (!ps_function_binary_op(interpreter, &left, &right, result, and_operator))
                TRACE_ERROR("BINARY");
            left.type = result->type;
            left.data = result->data;
        }
    } while (true);

    VISIT_END("AND2");
}

/**
 * Visit relational expression:
 *      simple_expression [ '<' | '<=' | '>' | '>=' | '=' | '<>' , simple_expression ]
 */
bool ps_visit_relational_expression(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
{
    VISIT_BEGIN("RELATIONAL_EXPRESSION", "");

    static ps_token_type relational_operators[] = {
        PS_TOKEN_LT,        PS_TOKEN_LE, PS_TOKEN_GT,
        PS_TOKEN_GE, PS_TOKEN_EQUAL,         PS_TOKEN_NE,
    };
    ps_value left = {.type = &ps_system_none, .data.v = NULL};
    ps_value right = {.type = &ps_system_none, .data.v = NULL};
    ps_token_type relational_operator = PS_TOKEN_NONE;

    if (!ps_visit_simple_expression(interpreter, mode, &left))
        TRACE_ERROR("RELATIONAL1");
    relational_operator = ps_parser_expect_token_types(
        interpreter->parser, sizeof(relational_operators) / sizeof(ps_token_type), relational_operators);
    if (relational_operator == PS_TOKEN_NONE)
    {
        if (mode == MODE_EXEC)
        {
            result->type = left.type;
            result->data = left.data;
        }
        VISIT_END("RELATIONAL1");
    }
    READ_NEXT_TOKEN;
    if (!ps_visit_simple_expression(interpreter, mode, &right))
        TRACE_ERROR("RELATIONAL2");
    if (mode == MODE_EXEC)
    {
        result->type = &ps_system_boolean;
        if (!ps_function_binary_op(interpreter, &left, &right, result, relational_operator))
            TRACE_ERROR("BINARY");
    }

    VISIT_END("RELATIONAL2");
}

/**
 * Visit simple expression:
 *      term [ '+' | '-' term ]*
 *  NB: 'OR' | 'XOR' are accounted by or_expression
 */
bool ps_visit_simple_expression(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
{
    VISIT_BEGIN("SIMPLE_EXPRESSION", "");

    static ps_token_type additive_operators[] = {
        PS_TOKEN_PLUS, PS_TOKEN_MINUS,
        // PS_TOKEN_OR, PS_TOKEN_XOR
    };
    ps_value left = {.type = result->type, .data.v = NULL};
    ps_value right = {.type = result->type, .data.v = NULL};
    ps_token_type additive_operator = PS_TOKEN_NONE;

    if (!ps_visit_term(interpreter, mode, &left))
        TRACE_ERROR("TERM");
    do
    {
        additive_operator = ps_parser_expect_token_types(
            interpreter->parser, sizeof(additive_operators) / sizeof(ps_token_type), additive_operators);
        if (additive_operator == PS_TOKEN_NONE)
        {
            if (mode == MODE_EXEC)
            {
                result->type = left.type;
                result->data = left.data;
            }
            VISIT_END("SIMPLE1");
        }
        READ_NEXT_TOKEN;
        if (!ps_visit_term(interpreter, mode, &right))
            TRACE_ERROR("TERM");
        if (mode == MODE_EXEC)
        {
            if (!ps_function_binary_op(interpreter, &left, &right, result, additive_operator))
                TRACE_ERROR("BINARY");
            left.type = result->type;
            left.data = result->data;
        }
    } while (true);

    VISIT_END("SIMPLE2");
}

/**
 * Visit term:
 *      factor [ '*' | '/' | 'DIV' | 'MOD' | 'AND' | 'SHL' | 'SHR' | 'AS' factor ]*
 */
bool ps_visit_term(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
{
    static ps_token_type multiplicative_operators[] = {
        PS_TOKEN_STAR, PS_TOKEN_SLASH, PS_TOKEN_DIV, PS_TOKEN_MOD,
        // PS_TOKEN_AND,  PS_TOKEN_SHL,   PS_TOKEN_SHR
    };

    VISIT_BEGIN("TERM", "");

    ps_value left = {.type = result->type, .data.v = NULL};
    ps_value right = {.type = result->type, .data.v = NULL};
    ps_token_type multiplicative_operator = PS_TOKEN_NONE;
    if (!ps_visit_factor(interpreter, mode, &left))
        TRACE_ERROR("FACTOR");
    do
    {
        multiplicative_operator = ps_parser_expect_token_types(
            interpreter->parser, sizeof(multiplicative_operators) / sizeof(ps_token_type), multiplicative_operators);
        if (multiplicative_operator == PS_TOKEN_NONE)
        {
            if (mode == MODE_EXEC)
            {
                result->type = left.type;
                result->data = left.data;
            }
            VISIT_END("1");
        }
        READ_NEXT_TOKEN;
        if (!ps_visit_factor(interpreter, mode, &right))
            TRACE_ERROR("FACTOR");
        if (mode == MODE_EXEC)
        {
            if (!ps_function_binary_op(interpreter, &left, &right, result, multiplicative_operator))
                TRACE_ERROR("BINARY");
            left.type = result->type;
            left.data = result->data;
        }
    } while (true);

    VISIT_END("2");
}

/**
 * Visit
 *  factor  = '(' , expression , ')'
 *          | variable_reference
 *          | constant_reference
 *          | function_call
 *          | string_value | char_value | integer_value | unsigned_value | real_value | boolean_value
 *          | [ '+' | '-' | 'NOT' ] , factor
 *          | nil
 *          ;
 */
bool ps_visit_factor(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
{
    VISIT_BEGIN("FACTOR", "");
    ps_value factor = {.type = &ps_system_none, .data.v = NULL};
    ps_identifier identifier;
    ps_symbol *symbol;
    ps_token_type unary_operator;

    switch (lexer->current_token.type)
    {
    case PS_TOKEN_LEFT_PARENTHESIS:
        READ_NEXT_TOKEN;
        if (!ps_visit_expression(interpreter, mode, result))
            TRACE_ERROR("EXPRESSION");
        EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_IDENTIFIER:
        // variable, constant, function
        COPY_IDENTIFIER(identifier);
        symbol = ps_interpreter_find_symbol(interpreter, &identifier, false);
        if (symbol == NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
        switch (symbol->kind)
        {
        case PS_SYMBOL_KIND_AUTO:
        case PS_SYMBOL_KIND_CONSTANT:
        case PS_SYMBOL_KIND_VARIABLE:
            if (mode == MODE_EXEC)
            {
                if (!ps_interpreter_copy_value(interpreter, symbol->value, result))
                    TRACE_ERROR("COPY");
            }
            READ_NEXT_TOKEN;
            break;
        case PS_SYMBOL_KIND_FUNCTION:
            if (!ps_visit_function_call(interpreter, mode, symbol, result))
                TRACE_ERROR("FUNCTION");
            break;
        default:
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
            break;
        }

        break;
    case PS_TOKEN_CHAR_VALUE:
        if (mode == MODE_EXEC)
        {
            result->type = &ps_system_char;
            result->data.c = lexer->current_token.value.c;
        }
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_INTEGER_VALUE:
        if (mode == MODE_EXEC)
        {
            result->type = &ps_system_integer;
            result->data.i = lexer->current_token.value.i;
        }
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_UNSIGNED_VALUE:
        if (mode == MODE_EXEC)
        {
            result->type = &ps_system_unsigned;
            result->data.u = lexer->current_token.value.u;
        }
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_REAL_VALUE:
        if (mode == MODE_EXEC)
        {
            result->type = &ps_system_real;
            result->data.r = lexer->current_token.value.r;
        }
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_BOOLEAN_VALUE:
        if (mode == MODE_EXEC)
        {
            result->type = &ps_system_boolean;
            result->data.b = lexer->current_token.value.b;
        }
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_PLUS:
        READ_NEXT_TOKEN;
        if (!ps_visit_factor(interpreter, mode, result))
            TRACE_ERROR("UNARY_PLUS");
        break;
    case PS_TOKEN_MINUS:
    case PS_TOKEN_NOT:
        unary_operator = lexer->current_token.type;
        READ_NEXT_TOKEN;
        if (!ps_visit_factor(interpreter, mode, &factor))
            TRACE_ERROR("UNARY_MINUS_NOT");
        if (mode == MODE_EXEC && !ps_function_unary_op(interpreter, &factor, result, unary_operator))
            TRACE_ERROR("UNARY");
        break;
    case PS_TOKEN_STRING_VALUE:
        if (mode == MODE_EXEC)
        {
            result->data.s = ps_string_heap_create(interpreter->string_heap, lexer->current_token.value.s);
            if (result->data.s == NULL)
            {
                interpreter->error = PS_ERROR_OUT_OF_MEMORY;
                TRACE_ERROR("STRING_VALUE");
            }
            result->type = &ps_system_string;
        }
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_NIL:
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        TRACE_ERROR("NIL");
    default:
        interpreter->error = PS_ERROR_UNEXPECTED_TOKEN;
        TRACE_ERROR("?");
    }

    VISIT_END("OK");
}

/**
 * Visit function call:
 *      identifier [ '(' , expression [ ',' , expression ]* ')' ]
 *  only 1 parameter for now and only "system" functions
 * Next steps:
 *  - get all parameters
 *  - check function signature
 *  - check function return type
 */
bool ps_visit_function_call(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol *function,
                            ps_value *result)
{
    VISIT_BEGIN("FUNCTION_CALL", "");

    ps_value arg = {.type = &ps_system_none, .data.v = NULL};
    bool null_arg = false;

    READ_NEXT_TOKEN;
    if (function == &ps_system_function_random)
    {
        // Random function can be called with 2 signatures:
        //  1. Random or Random() => Real
        //  2. Random(Integer|Unsigned) => Integer|Unsigned
        switch (lexer->current_token.type)
        {
        case PS_TOKEN_LEFT_PARENTHESIS:
            // Skip '(' and ')' or get parameter enclosed in parentheses
            READ_NEXT_TOKEN;
            if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
            {
                READ_NEXT_TOKEN;
                null_arg = true;
            }
            else
            {
                if (!ps_visit_expression(interpreter, mode, &arg))
                    TRACE_ERROR("PARAMETER");
                EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
                READ_NEXT_TOKEN;
            }
            break;
        case PS_TOKEN_SEMI_COLON:
        case PS_TOKEN_ELSE:
        case PS_TOKEN_END:
        case PS_TOKEN_UNTIL:
            // Statement terminators => OK
            null_arg = true;
            break;
        default:
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        }
    }
    else if (function == &ps_system_function_get_tick_count)
    {
        // Random function can be called with 2 signatures:
        //  1. Random or Random() => Real
        //  2. Random(Integer|Unsigned) => Integer|Unsigned
        // interpreter->debug = DEBUG_VERBOSE;
        switch (lexer->current_token.type)
        {
        case PS_TOKEN_LEFT_PARENTHESIS:
            // Skip '(' and ')'
            READ_NEXT_TOKEN;
            if (lexer->current_token.type != PS_TOKEN_RIGHT_PARENTHESIS)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
            null_arg = true;
            READ_NEXT_TOKEN;
            break;
        case PS_TOKEN_SEMI_COLON:
        case PS_TOKEN_ELSE:
        case PS_TOKEN_END:
        case PS_TOKEN_UNTIL:
            // Statement terminators => OK
            null_arg = true;
            break;
        default:
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        }
    }
    else
    {
        // all other functions have one argument for now
        EXPECT_TOKEN(PS_TOKEN_LEFT_PARENTHESIS);
        READ_NEXT_TOKEN;
        if (!ps_visit_expression(interpreter, mode, &arg))
            TRACE_ERROR("PARAMETER");
        EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
        READ_NEXT_TOKEN;
    }
    if (mode == MODE_EXEC)
    {
        interpreter->error = ps_function_exec(interpreter, function, null_arg ? NULL : &arg, result);
        if (interpreter->error != PS_ERROR_NONE)
            TRACE_ERROR("FUNCTION");
    }

    VISIT_END("OK");
}
