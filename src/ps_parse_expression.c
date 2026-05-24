/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>
#include <string.h>

#include "ps_array.h"
#include "ps_functions.h"
#include "ps_parse.h"
#include "ps_parser.h"
#include "ps_procedures.h"
#include "ps_string.h"
#include "ps_system.h"

#define MODE_EXEC 0
static int mode = MODE_EXEC;

/**
 *  This is the entry point for visiting all expressions.
 */
bool ps_parse_expression(ps_compiler *compiler, ps_ast_block *block, ps_ast_node **result)
{
    return ps_parse_or_expression(compiler, block, result);
}

/**
 * Visit
 *      or_expression = and_expression { ( 'OR' | 'XOR' ) and_expression }
 * Goal:
 *      make A < 1 OR A > 10 OR A = 5 OR ... work without parenthesis
 * AST:
 *      A               => A
 *      A or B          => binary_op(or, A, B)
 *      A or B xor C    => binary_op(xor, binary(or, A, B), C)
 */
bool ps_parse_or_expression(ps_compiler *compiler, ps_ast_block *block, ps_ast_node **result)
{
    PARSE_BEGIN("OR_EXPRESSION", "");

    static ps_token_type or_operators[] = {PS_TOKEN_OR, PS_TOKEN_XOR};
    static size_t or_operator_count = sizeof(or_operators) / sizeof(ps_token_type);
    ps_ast_node *left = NULL;
    ps_ast_node *right = NULL;
    ps_token_type or_operator = PS_TOKEN_NONE;

    uint16_t start_line = lexer->start_line;
    uint16_t start_column = lexer->start_column;
    if (!ps_parse_and_expression(compiler, block, &left))
        TRACE_ERROR("AND1");
    do
    {
        or_operator = ps_parser_expect_token_types(compiler->parser, or_operator_count, or_operators);
        if (or_operator == PS_TOKEN_NONE)
        {
            *result = left;
            PARSE_END("LEFT");
        }
        READ_NEXT_TOKEN
        if (!ps_parse_and_expression(compiler, block, &right))
            TRACE_ERROR("AND2");
        ps_operator_binary operator = ps_operator_binary_from_token(or_operator);
        if (operator == PS_OP_BINARY_INVALID)
        {
            ps_compiler_set_message(compiler, "Token %s (%d) has no matching AST binary operator",
                                    ps_token_get_keyword(and_operator), and_operator);
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        }
        left = ps_ast_create_binary_operation(start_line, start_column, operator, left, right);
        if (left == NULL)
            TRACE_ERROR("BINARY_OP");
    } while (true);

    *result = left;

    PARSE_END("RIGHT");
}

/**
 * Visit and expression:
 *      relational_expression { 'AND' relational_expression }
 * Goal:
 *      make A < 1 AND A > 10 AND B = 5 AND ... work without parenthesis
 *      ByteValue AND $0F should work too
 * AST:
 *      A               => A
 *      A and B         => binary_op(and, A, B)
 *      A and B and C   => binary_op(and, binary(and, A, B), C)
 */
bool ps_parse_and_expression(ps_compiler *compiler, ps_ast_block *block, ps_ast_node **result)
{
    PARSE_BEGIN("AND_EXPRESSION", "");

    static ps_token_type and_operators[] = {PS_TOKEN_AND};
    static size_t and_operator_count = sizeof(and_operators) / sizeof(ps_token_type);
    ps_ast_node *left = NULL;
    ps_ast_node *right = NULL;
    ps_token_type and_operator = PS_TOKEN_NONE;

    uint16_t start_line = lexer->start_line;
    uint16_t start_column = lexer->start_column;
    if (!ps_parse_relational_expression(compiler, block, &left))
        TRACE_ERROR("RELATIONAL1");
    do
    {
        and_operator = ps_parser_expect_token_types(compiler->parser, and_operator_count, and_operators);
        if (and_operator == PS_TOKEN_NONE)
        {
            *result = left;
            PARSE_END("AND1");
        }
        READ_NEXT_TOKEN
        if (!ps_parse_relational_expression(compiler, block, &right))
            TRACE_ERROR("RELATIONAL2");
        ps_operator_binary operator = ps_operator_binary_from_token(and_operator);
        if (operator == PS_OP_BINARY_INVALID)
        {
            ps_compiler_set_message(compiler, "Token %s (%d) has no matching AST binary operator",
                                    ps_token_get_keyword(and_operator), and_operator);
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        }
        left = ps_ast_create_binary_operation(start_line, start_column, operator, left, right);
        if (left == NULL)
            TRACE_ERROR("BINARY_OP");
    } while (true);

    *result = left;

    PARSE_END("AND2");
}

/**
 * Visit relational expression:
 *      simple_expression '<' | '<=' | '>' | '>=' | '=' | '<>' simple_expression
 * AST:
 *      A           => A
 *      A >= B      => binary_op(GE, A, B)
 */
bool ps_parse_relational_expression(ps_compiler *compiler, ps_ast_block *block, ps_ast_node **result)
{
    PARSE_BEGIN("RELATIONAL_EXPRESSION", "");

    static ps_token_type relational_operators[] = {
        // <         <=           >            >=           =            <>
        PS_TOKEN_LT, PS_TOKEN_LE, PS_TOKEN_GT, PS_TOKEN_GE, PS_TOKEN_EQ, PS_TOKEN_NE,
    };
    ps_ast_node *left = NULL;
    ps_ast_node *right = NULL;
    ps_token_type relational_operator = PS_TOKEN_NONE;

    uint16_t start_line = lexer->start_line;
    uint16_t start_column = lexer->start_column;
    if (!ps_parse_simple_expression(compiler, block, &left))
        TRACE_ERROR("RELATIONAL1");
    // No loop, only one relational operator allowed, no a <= b <= c
    relational_operator = ps_parser_expect_token_types(
        compiler->parser, sizeof(relational_operators) / sizeof(ps_token_type), relational_operators);
    if (relational_operator == PS_TOKEN_NONE)
    {
        *result = left;
        PARSE_END("RELATIONAL1");
    }
    READ_NEXT_TOKEN
    if (!ps_parse_simple_expression(compiler, block, &right))
        TRACE_ERROR("RELATIONAL2");
    ps_operator_binary operator = ps_operator_binary_from_token(relational_operator);
    if (operator == PS_OP_BINARY_INVALID)
    {
        ps_compiler_set_message(compiler, "Token %s (%d) has no matching AST binary operator",
                                ps_token_get_keyword(relational_operator), relational_operator);
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }
    *result = ps_ast_create_binary_operation(start_line, start_column, operator, left, right);
    PARSE_END("RELATIONAL2");
}

/**
 * Visit simple expression:
 *      term [ '+' | '-' term ]*
 * NB: 'OR' | 'XOR' are accounted by or_expression
 * AST:
 *      A           => A
 *      A + B       => binary(+, A, B)
 *      A + B + C   => binary(+, binary(+, A, B), C)
 */
bool ps_parse_simple_expression(ps_compiler *compiler, ps_ast_block *block, ps_ast_node **result)
{
    PARSE_BEGIN("SIMPLE_EXPRESSION", "");

    static ps_token_type additive_operators[] = {PS_TOKEN_PLUS, PS_TOKEN_MINUS};
    size_t additive_operator_count = sizeof(additive_operators) / sizeif(ps_token_type);
    ps_ast_node *left = NULL;
    ps_ast_node *right = NULL;
    ps_token_type additive_operator = PS_TOKEN_NONE;

    uint16_t start_line = lexer->start_line;
    uint16_t start_column = lexer->start_column;
    if (!ps_parse_term(compiler, block, &left))
        TRACE_ERROR("TERM");
    do
    {
        additive_operator = ps_parser_expect_token_types(compiler->parser, additive_operator_count, additive_operators);
        if (additive_operator == PS_TOKEN_NONE)
        {
            if (mode == MODE_EXEC && !ps_compiler_copy_value(compiler, &left, result))
                TRACE_ERROR("COPY");
            PARSE_END("SIMPLE1");
        }
        READ_NEXT_TOKEN
        if (!ps_parse_term(compiler, block, &right))
            TRACE_ERROR("TERM");
        // // Promote to real if one operand is real
        // if (left.type->value->data.t->base == PS_TYPE_REAL || right.type->value->data.t->base == PS_TYPE_REAL)
        // {
        //     factor.type = &ps_system_real;
        // }
        ps_operator_binary operator = ps_operator_binary_from_token(additive_operator);
        if (operator == PS_OP_BINARY_INVALID)
        {
            ps_compiler_set_message(compiler, "Token %s (%d) has no matching AST binary operator",
                                    ps_token_get_keyword(additive_operator), additive_operator);
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        }
        left = ps_ast_create_binary_operation(start_line, start_column, operator, left, right);
        if (left == NULL)
            TRACE_ERROR("BINARY_OP");
    } while (true);

    PARSE_END("SIMPLE2");
}

/**
 * Visit term:
 *      factor [ '*' | '/' | 'DIV' | 'MOD' | 'AND' | 'SHL' | 'SHR' | 'AS' factor ]*
 */
bool ps_parse_term(ps_compiler *compiler, ps_ast_block *block, ps_ast_node **result)
{
    PARSE_BEGIN("TERM", "");

    static ps_token_type multiplicative_operators[] = {PS_TOKEN_STAR, PS_TOKEN_SLASH, PS_TOKEN_DIV,
                                                       PS_TOKEN_MOD,  PS_TOKEN_SHL,   PS_TOKEN_SHR};
    size_t multiplicative_operator_count = sizeof(multiplicative_operators) / sizeof(ps_token_type);
    ps_ast_node *left = NULL;
    ps_ast_node *right = NULL;
    ps_token_type multiplicative_operator = PS_TOKEN_NONE;

    uint16_t start_line = lexer->start_line;
    uint16_t start_column = lexer->start_column;
    if (!ps_parse_factor(compiler, block, &left))
        TRACE_ERROR("FACTOR");
    do
    {
        multiplicative_operator =
            ps_parser_expect_token_types(compiler->parser, multiplicative_operator_count, multiplicative_operators);
        if (multiplicative_operator == PS_TOKEN_NONE)
        {
            *result = left;
            PARSE_END("TERM1");
        }
        READ_NEXT_TOKEN
        if (!ps_parse_factor(compiler, block, &right))
            TRACE_ERROR("FACTOR");
        // // For multiplication/division, promote to real if one operand is real
        // if ((multiplicative_operator == PS_TOKEN_STAR || multiplicative_operator == PS_TOKEN_SLASH) &&
        //     (left.type->value->data.t->base == PS_TYPE_REAL || right.type->value->data.t->base == PS_TYPE_REAL))
        // {
        //     factor.type = &ps_system_real;
        // }
        ps_operator_binary operator = ps_operator_binary_from_token(multiplicative_operator);
        if (operator == PS_OP_BINARY_INVALID)
        {
            ps_compiler_set_message(compiler, "Token %s (%d) has no matching AST binary operator",
                                    ps_token_get_keyword(multiplicative_operator), multiplicative_operator);
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        }
        left = ps_ast_create_binary_operation(start_line, start_column, operator, left, right);
        if (left == NULL)
            TRACE_ERROR("BINARY_OP");
    } while (true);

    PARSE_END("TERM2");
}

bool ps_parse_factor_identifier_array(ps_compiler *compiler, ps_ast_block *block, const ps_symbol *symbol,
                                      ps_value *result)
{
    PARSE_BEGIN("FACTOR", "ARRAY");
    RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
    // const ps_type_definition *type_def = ps_array_get_type_def(symbol->value->type);
    // if (type_def == NULL)
    //     RETURN_ERROR(PS_ERROR_TYPE_MISMATCH)
    // READ_NEXT_TOKEN
    // if (lexer->current_token.type == PS_TOKEN_LEFT_BRACKET)
    // {
    //     ps_value index = {.type = &ps_system_none /*type_def->def.a.item_type*/, .allocated = false, .data.v = NULL};
    //     READ_NEXT_TOKEN
    //     if (!ps_parse_expression(compiler,block, &index))
    //     {
    //         ps_compiler_set_message(compiler, "Index is invalid");
    //         TRACE_ERROR("INDEX")
    //     }
    //     EXPECT_TOKEN(PS_TOKEN_RIGHT_BRACKET)
    //     if (mode == MODE_EXEC)
    //     {
    //         ps_error error = ps_array_get_value(symbol, &index, result, compiler->range_check);
    //         if (error != PS_ERROR_NONE)
    //         {
    //             ps_compiler_set_message(compiler, "Can't get array value for index %s",
    //                                        ps_value_get_debug_string(&index));
    //             RETURN_ERROR(error)
    //         }
    //     }
    //     READ_NEXT_TOKEN
    // }

    // PARSE_END("OK")
}

/**
 * @brief Visit constant or variable reference or function call
 * @details
 * Actual:
 *      constant reference = identifier
 *      variable reference = identifier [ '[' expression ']' ]
 *      function call      = identifier [ '(' [ expression [ ',' expression ]* ]')' ]
 * Next step:
 *  multi-dimensional arrays instead of vectors
 *      variable reference = identifier [ '[' expression [ ',' expression ]* ']' ]
 */
bool ps_parse_factor_identifier(ps_compiler *compiler, ps_ast_block *block, const char *identifier,
                                ps_ast_node **result)
{
    PARSE_BEGIN("FACTOR", "IDENTIFIER");

    uint16_t start_line = lexer->start_line;
    uint16_t start_column = lexer->start_column;
    ps_symbol *symbol = ps_compiler_find_symbol(compiler, identifier, false);
    if (symbol == NULL)
        RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
    switch (symbol->kind)
    {
    case PS_SYMBOL_KIND_AUTO:
    case PS_SYMBOL_KIND_CONSTANT:
    case PS_SYMBOL_KIND_VARIABLE:
        if (compiler->debug >= DEBUG_VERBOSE)
        {
            fprintf(stderr, "%cINFO\tFACTOR: identifier '%s' is a '%s' of type '%s'\n", mode == MODE_EXEC ? '*' : ' ',
                    symbol->name, ps_symbol_get_kind_name(symbol->kind),
                    ps_type_definition_get_name(symbol->value->type->value->data.t));
        }
        if (ps_value_is_array(symbol->value))
        {
            if (!ps_parse_factor_identifier_array(compiler, block, symbol, result))
                TRACE_ERROR("ARRAY")
        }
        else
        {
            // factor.type = symbol->value->type;

            READ_NEXT_TOKEN
        }
        break;
    case PS_SYMBOL_KIND_FUNCTION:
        if (!ps_parse_function_call(compiler, block, symbol, result))
            TRACE_ERROR("FUNCTION")
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }

    PARSE_END("OK")
}

/**
 * @brief Visit factor
 * @details
 *  factor  = '(' , expression , ')'
 *          | variable_reference
 *          | constant_reference
 *          | function_call
 *          | string_value | char_value | integer_value | unsigned_value | real_value | boolean_value
 *          | [ '+' | '-' | 'NOT' ] factor
 * Next steps:
 *          | nil
 */
bool ps_parse_factor(ps_compiler *compiler, ps_ast_block *block, ps_ast_node **result)
{
    PARSE_BEGIN("FACTOR", "");

    ps_value factor_value = {.type = &ps_system_none, .data.v = NULL};
    ps_identifier identifier;
    ps_token_type unary_operator;

    uint16_t start_line = lexer->start_line;
    uint16_t start_column = lexer->start_column;
    switch (lexer->current_token.type)
    {
    // ***Parenthesized expression ***
    case PS_TOKEN_LEFT_PARENTHESIS:
        READ_NEXT_TOKEN
        if (!ps_parse_expression(compiler, block, result))
            TRACE_ERROR("EXPRESSION");
        EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
        READ_NEXT_TOKEN
        break;
    // *** Identifier: variable, constant, function ***
    case PS_TOKEN_IDENTIFIER:
        COPY_IDENTIFIER(identifier)
        if (!ps_parse_factor_identifier(compiler, block, identifier, result))
            TRACE_ERROR("IDENTIFIER")
        break;
    // ***Literal values ***
    case PS_TOKEN_CHAR_VALUE:
        factor_value.type = &ps_system_char;
        factor_value.data.c = lexer->current_token.value.c;
        READ_NEXT_TOKEN
        break;
    case PS_TOKEN_INTEGER_VALUE:
        factor_value.type = &ps_system_integer;
        factor_value.data.i = lexer->current_token.value.i;
        READ_NEXT_TOKEN
        break;
    case PS_TOKEN_UNSIGNED_VALUE:
        factor_value.type = &ps_system_unsigned;
        factor_value.data.u = lexer->current_token.value.u;
        READ_NEXT_TOKEN
        break;
    case PS_TOKEN_REAL_VALUE:
        factor_value.type = &ps_system_real;
        factor_value.data.r = lexer->current_token.value.r;
        READ_NEXT_TOKEN
        break;
    case PS_TOKEN_BOOLEAN_VALUE:
        factor_value.type = &ps_system_boolean;
        factor_value.data.b = lexer->current_token.value.b;
        READ_NEXT_TOKEN
        break;
    case PS_TOKEN_STRING_VALUE:
        factor_value.type = &ps_system_string;
        factor_value.data.s = ps_string_heap_create(compiler->string_heap, lexer->current_token.value.s);
        if (factor_value.data.s == NULL)
        {
            ps_compiler_set_message(compiler, "Failed to create string value: %s", strerror(errno));
            compiler->error = ps_error_map_errno();
            TRACE_ERROR("STRING_VALUE")
        }
        READ_NEXT_TOKEN
        break;
    case PS_TOKEN_NIL:
        compiler->error = PS_ERROR_NOT_IMPLEMENTED;
        TRACE_ERROR("NIL");
    // *** Unary operators ***
    case PS_TOKEN_PLUS:
        READ_NEXT_TOKEN
        if (!ps_parse_factor(compiler, block, result))
            TRACE_ERROR("UNARY_PLUS");
        break;
    case PS_TOKEN_MINUS:
    case PS_TOKEN_NOT:
        unary_operator = lexer->current_token.type;
        READ_NEXT_TOKEN
        ps_ast_node *operand = NULL;
        if (!ps_parse_factor(compiler, block, &operand))
            TRACE_ERROR("UNARY_MINUS_NOT");
        *result = ps_ast_create_unary_operation(start_line, start_column,
                                                ps_operator_unary_from_token(lexer->current_token.type), &operand);
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }

    if (factor_value.type != &ps_system_none)
    {
        *result = ps_ast_create_rvalue_const(start_line, start_column, factor_value);
        if (*result == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
    }

    PARSE_END("OK")
}

bool ps_parse_function_call_random(ps_compiler *compiler, ps_ast_block *block, ps_ast_node **result, int *arg_count,
                                   ps_value *arg1)
{
    PARSE_BEGIN("FUNCTION_CALL", "RANDOM");

    // Random function can be called with 3 signatures:
    //  1. Random or Random() => Real from 0.0 to 1.0 excluded
    //  2. Random(Integer) => Integer
    //  3. Random(Unsigned) => Unsigned
    if (lexer->current_token.type == PS_TOKEN_LEFT_PARENTHESIS)
    {
        // Skip '(' and ')' or get parameter enclosed in parentheses
        READ_NEXT_TOKEN
        if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
        {
            *arg_count = 0;
            factor.type = &ps_system_real;
            READ_NEXT_TOKEN
        }
        else
        {
            *arg_count = 1;
            if (!ps_parse_expression(compiler, block, arg1))
                TRACE_ERROR("PARAMETER");
            if (mode == MODE_EXEC && arg1->type != &ps_system_integer && arg1->type != &ps_system_unsigned)
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
            EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
            factor.type = arg1->type;
            READ_NEXT_TOKEN
        }
    }
    else
    {
        *arg_count = 0;
        factor.type = &ps_system_real;
    }

    PARSE_END("OK")
}

bool ps_parse_function_call_low_high(ps_compiler *compiler, ps_ast_block *block, ps_symbol **symbol)
{
    PARSE_BEGIN("FUNCTION_CALL", "LOW_HIGH")

    // Low and High functions have one "symbolic" argument, i.e. Low(Days) or High(Day)
    EXPECT_TOKEN(PS_TOKEN_LEFT_PARENTHESIS)
    READ_NEXT_TOKEN
    if (lexer->current_token.type != PS_TOKEN_IDENTIFIER && lexer->current_token.type != PS_TOKEN_INTEGER &&
        lexer->current_token.type != PS_TOKEN_UNSIGNED && lexer->current_token.type != PS_TOKEN_CHAR)
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    ps_identifier identifier = {0};
    COPY_IDENTIFIER(identifier)
    *symbol = ps_compiler_find_symbol(compiler, identifier, false);
    if (*symbol == NULL)
        RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND)
    READ_NEXT_TOKEN
    EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS)
    READ_NEXT_TOKEN

    PARSE_END("OK")
}

bool ps_parse_function_call_power(ps_compiler *compiler, ps_ast_block *block, ps_value *arg1, ps_value *arg2)
{
    PARSE_BEGIN("FUNCTION_CALL", "POWER");

    EXPECT_TOKEN(PS_TOKEN_LEFT_PARENTHESIS)
    READ_NEXT_TOKEN
    if (!ps_parse_expression(compiler, block, arg1))
        TRACE_ERROR("ARG1")
    if (!ps_value_is_number(arg1) && !ps_value_is_real(arg1))
        RETURN_ERROR(PS_ERROR_EXPECTED_NUMBER)
    EXPECT_TOKEN(PS_TOKEN_COMMA)
    READ_NEXT_TOKEN
    if (!ps_parse_expression(compiler, block, arg2))
        TRACE_ERROR("ARG2")
    if (!ps_value_is_number(arg2) && !ps_value_is_real(arg2))
        RETURN_ERROR(PS_ERROR_EXPECTED_NUMBER)
    EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS)
    READ_NEXT_TOKEN

    PARSE_END("OK")
}

/**
 * Visit system function call:
 *      identifier [ '(' , expression | variable_reference [ ',' , expression | variable_reference ]* ')' ]
 */
bool ps_parse_function_call_system(ps_compiler *compiler, ps_ast_block *block, const ps_symbol *function,
                                   ps_value *result)
{
    PARSE_BEGIN("FUNCTION_CALL", "SYSTEM");

    ps_value arg1 = {.type = &ps_system_none, .data.v = NULL};
    ps_value arg2 = {.type = &ps_system_none, .data.v = NULL};
    ps_symbol *symbol = NULL;
    int arg_count = 0;

    // Handle specific function types with dedicated handlers
    if (function == &ps_system_function_random)
    {
        if (!ps_parse_function_call_random(compiler, block, result, &arg_count, &arg1))
            TRACE_ERROR("RANDOM")
    }
    else if (function == &ps_system_function_get_tick_count)
    {
        // No arguments, skip optional '(' and ')'
        if (lexer->current_token.type == PS_TOKEN_LEFT_PARENTHESIS)
        {
            READ_NEXT_TOKEN
            EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
            READ_NEXT_TOKEN
        }
        arg_count = 0;
        factor.type = &ps_system_unsigned;
    }
    else if (function == &ps_system_function_low || function == &ps_system_function_high)
    {
        arg_count = -1;
        if (!ps_parse_function_call_low_high(compiler, block, &symbol))
            TRACE_ERROR("LOW_HIGH")
    }
    else if (function == &ps_system_function_power)
    {
        // Power function has two "by value" numeric arguments
        arg_count = 2;
        if (!ps_parse_function_call_power(compiler, block, &arg1, &arg2))
            TRACE_ERROR("POWER")
    }
    else
    {
        // all other functions have one "by value" argument for now
        // examples: Ord, Chr, Pred, Succ, Sin, Cos, ...
        arg_count = 1;
        EXPECT_TOKEN(PS_TOKEN_LEFT_PARENTHESIS);
        READ_NEXT_TOKEN
        if (!ps_parse_expression(compiler, block, &arg1))
            TRACE_ERROR("ARG");
        EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
        READ_NEXT_TOKEN
    }

    // Execute the function based on its type
    if (mode == MODE_EXEC)
    {
        ps_error error = PS_ERROR_NONE;
        switch (arg_count)
        {
        case -1:
            error = ps_function_exec_1arg_s(compiler, function, symbol, result);
            break;
        case 0:
            error = ps_function_exec_1arg(compiler, function, NULL, result);
            break;
        case 1:
            error = ps_function_exec_1arg(compiler, function, &arg1, result);
            break;
        case 2:
            error = ps_function_exec_2args(compiler, function, &arg1, &arg2, result);
            break;
        default:
            error = PS_ERROR_INVALID_PARAMETERS;
            break;
        }
        if (error != PS_ERROR_NONE)
        {
            compiler->error = error;
            TRACE_ERROR("SYSTEM_FUNCTION")
        }
    }

    PARSE_END("OK")
}

/**
 * Visit system or user function call:
 *      identifier [ '(' , expression | variable_reference [ ',' , expression | variable_reference ]* ')' ]
 */
bool ps_parse_function_call(ps_compiler *compiler, ps_ast_block *block, ps_symbol *function, ps_ast_node **result)
{
    PARSE_BEGIN("FUNCTION_CALL", "");

    READ_NEXT_TOKEN
    if (function->system)
    {
        if (!ps_parse_function_call_system(compiler, block, function, result))
            TRACE_ERROR("SYSTEM")
    }
    else
    {
        // User defined function
        if (!ps_parse_procedure_or_function_call(compiler, block, function, result))
            TRACE_ERROR("FUNCTION_CALL");
    }

    PARSE_END("OK")
}

/**
 * Visit constant expression:
 *      [ '-' ] INTEGER_VALUE
 *      | UNSIGNED_VALUE
 *      | CHAR_VALUE
 *      | [ '-' ] REAL_VALUE
 *      | BOOLEAN_VALUE
 *      | [ '-' ] IDENTIFIER
 *      | STRING_VALUE
 */
bool ps_parse_constant_expression(ps_compiler *compiler, ps_ast_block *block, ps_value *constant)
{
    PARSE_BEGIN("CONSTANT_EXPRESSION", "");
    bool negate = false;

    ps_identifier identifier = {0};
    ps_symbol *symbol = NULL;

    // For now only keep track of '-' so "Const Foo = -4;" or "Const Bar = -Foo;" work as expected
    if (lexer->current_token.type == PS_TOKEN_MINUS)
    {
        negate = true;
        READ_NEXT_TOKEN
        if (lexer->current_token.type != PS_TOKEN_IDENTIFIER && lexer->current_token.type != PS_TOKEN_INTEGER_VALUE &&
            lexer->current_token.type != PS_TOKEN_REAL_VALUE && lexer->current_token.type != PS_TOKEN_UNSIGNED_VALUE)
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }
    switch (lexer->current_token.type)
    {
    case PS_TOKEN_INTEGER_VALUE:
        constant->type = &ps_system_integer;
        constant->data.i = negate ? -lexer->current_token.value.i : lexer->current_token.value.i;
        break;
    case PS_TOKEN_REAL_VALUE:
        constant->type = &ps_system_real;
        constant->data.r = negate ? -lexer->current_token.value.r : lexer->current_token.value.r;
        break;
    case PS_TOKEN_UNSIGNED_VALUE:
        if (negate)
        {
            if (constant->data.u > PS_INTEGER_MAX)
                RETURN_ERROR(PS_ERROR_OUT_OF_RANGE)
            constant->type = &ps_system_integer;
            constant->data.i = -(ps_integer)lexer->current_token.value.u;
        }
        else
        {
            constant->type = &ps_system_unsigned;
            constant->data.u = lexer->current_token.value.u;
        }
        break;
    case PS_TOKEN_CHAR_VALUE:
        constant->type = &ps_system_char;
        constant->data.c = lexer->current_token.value.c;
        break;
    case PS_TOKEN_BOOLEAN_VALUE:
        constant->type = &ps_system_boolean;
        constant->data.b = lexer->current_token.value.b;
        break;
    case PS_TOKEN_STRING_VALUE:
        constant->type = &ps_system_string;
        constant->data.s = ps_string_heap_create(compiler->string_heap, lexer->current_token.value.s);
        if (constant->data.s == NULL)
        {
            compiler->error = PS_ERROR_OUT_OF_MEMORY;
            TRACE_ERROR("STRING_VALUE")
        }
        break;
    case PS_TOKEN_IDENTIFIER:
        COPY_IDENTIFIER(identifier)
        symbol = ps_compiler_find_symbol(compiler, block, identifier, false);
        if (symbol == NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
        if (symbol->kind != PS_SYMBOL_KIND_CONSTANT)
            RETURN_ERROR(PS_ERROR_EXPECTED_CONSTANT);
        constant->type = symbol->value->type;
        constant->data = symbol->value->data;
        if (negate)
        {
            switch (ps_value_get_type(constant))
            {
            case PS_TYPE_INTEGER:
                constant->data.i = -constant->data.i;
                break;
            case PS_TYPE_UNSIGNED:
                if (constant->data.u > PS_INTEGER_MAX)
                    RETURN_ERROR(PS_ERROR_OUT_OF_RANGE)
                constant->type = &ps_system_integer;
                constant->data.i = -(ps_integer)constant->data.u;
                break;
            case PS_TYPE_REAL:
                constant->type = &ps_system_real;
                constant->data.r = -constant->data.r;
                break;
            default:
                RETURN_ERROR(PS_ERROR_EXPECTED_NUMBER)
            }
        }
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }
    READ_NEXT_TOKEN

    PARSE_END("OK")
}
