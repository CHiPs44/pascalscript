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

#define VISIT_BEGIN(__VISIT__, __PLUS__)                                                                               \
    ps_lexer *lexer = ps_parser_get_lexer(interpreter->parser);                                                           \
    static char *visit = __VISIT__;                                                                                    \
    if (interpreter->trace)                                                                                               \
    {                                                                                                                  \
        fprintf(stderr, "%*cBEGIN\t%-32s %-32s ", (interpreter->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ', visit, \
                __PLUS__);                                                                                             \
        ps_token_debug(stderr, "BEGIN", &lexer->current_token);                                                        \
    }
#define VISIT_END(__PLUS__)                                                                                            \
    if (interpreter->trace)                                                                                               \
    {                                                                                                                  \
        fprintf(stderr, "%*cEND\t%-32s %-32s ", (interpreter->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ', visit,   \
                __PLUS__);                                                                                             \
        ps_token_debug(stderr, "END", &lexer->current_token);                                                          \
    }                                                                                                                  \
    return true;
#define READ_NEXT_TOKEN                                                                                                \
    {                                                                                                                  \
        if (!ps_lexer_read_token(lexer))                                                                               \
            return false;                                                                                              \
        if (interpreter->trace)                                                                                           \
        {                                                                                                              \
            fprintf(stderr, "%*cTOKEN\t%-32s %-32s ", (interpreter->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ',    \
                    "", "");                                                                                           \
            ps_token_debug(stderr, "NEXT", &lexer->current_token);                                                     \
        }                                                                                                              \
    }
#define EXPECT_TOKEN(__PS_TOKEN_TYPE__)                                                                                \
    if (!ps_parser_expect_token_type(interpreter->parser, __PS_TOKEN_TYPE__))                                             \
    return false
#define RETURN_ERROR(__PS_ERROR__)                                                                                     \
    {                                                                                                                  \
        if (interpreter->trace)                                                                                           \
        {                                                                                                              \
            fprintf(stderr, "%*cRETURN\t%-32s %-8d ", (interpreter->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ',    \
                    visit, __PS_ERROR__);                                                                              \
            ps_token_debug(stderr, "RETURN", &lexer->current_token);                                                   \
        }                                                                                                              \
        return ps_interpreter_return_error(interpreter, __PS_ERROR__);                                                 \
    }
#define COPY_IDENTIFIER(__IDENTIFIER__)                                                                                \
    strncpy(__IDENTIFIER__, lexer->current_token.value.identifier, PS_IDENTIFIER_LEN)
#define TRACE_ERROR(__PLUS__)                                                                                          \
    {                                                                                                                  \
        if (interpreter->trace)                                                                                           \
        {                                                                                                              \
            fprintf(stderr, "%*cERROR\t%-32s %-32s ", (interpreter->level - 1) * 8 - 1, mode == MODE_EXEC ? '*' : ' ',    \
                    visit, __PLUS__);                                                                                  \
            ps_token_debug(stderr, "TRACE", &lexer->current_token);                                                    \
        }                                                                                                              \
        return false;                                                                                                  \
    }
#define TRACE_CURSOR                                                                                                   \
    if (interpreter->trace)                                                                                               \
    {                                                                                                                  \
        uint16_t line = 0;                                                                                             \
        uint8_t column = 0;                                                                                            \
        if (!ps_lexer_get_cursor(lexer, &line, &column))                                                               \
            TRACE_ERROR("CURSOR!");                                                                                    \
        fprintf(stderr, "%*cCURSOR\t*** LINE=%d, COLUMN=%d ***\n", (interpreter->level - 1) * 8 - 1,                      \
                mode == MODE_EXEC ? '*' : ' ', line, column);                                                          \
        ps_token_debug(stderr, "TRACE", &lexer->current_token);                                                        \
    }

// /* Forward declarations */
// bool ps_parse_expression(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result);
// bool ps_parse_statement_list(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_token_type stop);
// bool ps_parse_statement_or_compound_statement(ps_interpreter *interpreter, ps_interpreter_mode mode);
// bool ps_parse_statement(ps_interpreter *interpreter, ps_interpreter_mode mode);
// bool ps_parse_procedure_or_function(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol_kind kind);
// bool ps_parse_block(ps_interpreter *interpreter, ps_interpreter_mode mode);

// /**
//  * Parse
//  *      function_call = identifier [ '(' , expression [ ',' , expression ]* ')' ]
//  *  only 1 parameter for now and "system" functions
//  * TODO
//  *  - get all parameters
//  *  - check function signature
//  *  - check function return type
//  */
// bool ps_parse_function_call(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol *symbol, ps_value *result)
// {
//     VISIT_BEGIN("FUNCTION_CALL", "");
//     ps_value arg = {.type = ps_system_none.value->data.t, .data.v = NULL};
//     bool null_arg = false;

//     READ_NEXT_TOKEN;
//     if (symbol == &ps_system_function_random)
//     {
//         // Random function can be called with 2 signatures:
//         //  1. Random or Random() => Real
//         //  2. Random(Integer|Unsigned) => Integer|Unsigned
//         switch (lexer->current_token.type)
//         {
//         case PS_TOKEN_LEFT_PARENTHESIS:
//             // Skip '(' and ')' or get parameter enclosed in parentheses
//             READ_NEXT_TOKEN;
//             if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
//             {
//                 READ_NEXT_TOKEN;
//                 null_arg = true;
//             }
//             else
//             {
//                 if (!ps_parse_expression(interpreter, mode, &arg))
//                     TRACE_ERROR("PARAMETER");
//                 EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
//                 READ_NEXT_TOKEN;
//             }
//             break;
//         case PS_TOKEN_SEMI_COLON:
//         case PS_TOKEN_ELSE:
//         case PS_TOKEN_END:
//         case PS_TOKEN_UNTIL:
//             // Statement terminators => OK
//             null_arg = true;
//             break;
//         default:
//             RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
//         }
//     }
//     else
//     {
//         // all other functions have one argument for now
//         EXPECT_TOKEN(PS_TOKEN_LEFT_PARENTHESIS);
//         READ_NEXT_TOKEN;
//         if (!ps_parse_expression(interpreter, mode, &arg))
//             TRACE_ERROR("PARAMETER");
//         EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
//         READ_NEXT_TOKEN;
//     }
//     if (mode == MODE_EXEC)
//     {
//         if (!ps_function_exec(interpreter, symbol, null_arg ? NULL : &arg, result))
//             TRACE_ERROR("FUNCTION");
//     }

//     VISIT_END("OK");
// }

// /**
//  * Parse
//  *  factor  = '(' , expression , ')'
//  *          | variable_reference
//  *          | constant_reference
//  *          | function_call
//  *          | string_value | char_value | integer_value | unsigned_value | real_value | boolean_value
//  *          | [ '+' | '-' | 'NOT' ] , factor
//  *          | nil
//  *          ;
//  */
// bool ps_parse_factor(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
// {
//     VISIT_BEGIN("FACTOR", "");
//     ps_value factor = {.type = ps_system_none.value->data.t, .data.v = NULL};
//     ps_identifier identifier;
//     ps_symbol *symbol;
//     ps_token_type unary_operator;

//     switch (lexer->current_token.type)
//     {
//     case PS_TOKEN_LEFT_PARENTHESIS:
//         READ_NEXT_TOKEN;
//         if (!ps_parse_expression(interpreter, mode, result))
//             TRACE_ERROR("EXPRESSION");
//         EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
//         READ_NEXT_TOKEN;
//         break;
//     case PS_TOKEN_IDENTIFIER:
//         // variable, constant, function
//         COPY_IDENTIFIER(identifier);
//         symbol = ps_interpreter_find_symbol(interpreter, &identifier, false);
//         if (symbol == NULL)
//             RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
//         switch (symbol->kind)
//         {
//         case PS_SYMBOL_KIND_AUTO:
//         case PS_SYMBOL_KIND_CONSTANT:
//         case PS_SYMBOL_KIND_VARIABLE:
//             if (mode == MODE_EXEC)
//             {
//                 if (interpreter->debug)
//                 {
//                     ps_symbol_debug(stderr, "SYMBOL\t", symbol);
//                     ps_value_debug(stderr, "RESULT\t", result);
//                 }
//                 if (!ps_interpreter_copy_value(interpreter, symbol->value, result))
//                     TRACE_ERROR("COPY");
//             }
//             READ_NEXT_TOKEN;
//             break;
//         case PS_SYMBOL_KIND_FUNCTION:
//             if (!ps_parse_function_call(interpreter, mode, symbol, result))
//                 TRACE_ERROR("FUNCTION");
//             break;
//         default:
//             RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
//             break;
//         }

//         break;
//     case PS_TOKEN_CHAR_VALUE:
//         if (mode == MODE_EXEC)
//         {
//             result->type = ps_system_char.value->data.t;
//             result->data.c = lexer->current_token.value.c;
//         }
//         READ_NEXT_TOKEN;
//         break;
//     case PS_TOKEN_INTEGER_VALUE:
//         if (mode == MODE_EXEC)
//         {
//             result->type = ps_system_integer.value->data.t;
//             result->data.i = lexer->current_token.value.i;
//         }
//         READ_NEXT_TOKEN;
//         break;
//     case PS_TOKEN_UNSIGNED_VALUE:
//         if (mode == MODE_EXEC)
//         {
//             result->type = ps_system_unsigned.value->data.t;
//             result->data.u = lexer->current_token.value.u;
//         }
//         READ_NEXT_TOKEN;
//         break;
//     case PS_TOKEN_REAL_VALUE:
//         if (mode == MODE_EXEC)
//         {
//             result->type = ps_system_real.value->data.t;
//             result->data.r = lexer->current_token.value.r;
//         }
//         READ_NEXT_TOKEN;
//         break;
//     case PS_TOKEN_BOOLEAN_VALUE:
//         if (mode == MODE_EXEC)
//         {
//             result->type = ps_system_boolean.value->data.t;
//             result->data.b = lexer->current_token.value.b;
//         }
//         READ_NEXT_TOKEN;
//         break;
//     case PS_TOKEN_PLUS:
//         READ_NEXT_TOKEN;
//         if (!ps_parse_factor(interpreter, mode, result))
//             TRACE_ERROR("UNARY_PLUS");
//         break;
//     case PS_TOKEN_MINUS:
//     case PS_TOKEN_NOT:
//         unary_operator = lexer->current_token.type;
//         READ_NEXT_TOKEN;
//         if (!ps_parse_factor(interpreter, mode, &factor))
//             TRACE_ERROR("UNARY_MINUS_NOT");
//         if (mode == MODE_EXEC && !ps_function_unary_op(interpreter, &factor, result, unary_operator))
//             TRACE_ERROR("UNARY");
//         break;
//     case PS_TOKEN_STRING_VALUE:
//         if (mode == MODE_EXEC)
//         {
//             result->data.s = ps_string_heap_create(interpreter->string_heap, lexer->current_token.value.s);
//             if (result->data.s == NULL)
//             {
//                 interpreter->error = PS_ERROR_OUT_OF_MEMORY;
//                 TRACE_ERROR("STRING_VALUE");
//             }
//             result->type = ps_system_string.value->data.t;
//         }
//         READ_NEXT_TOKEN;
//         break;
//     case PS_TOKEN_NIL:
//         interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
//         TRACE_ERROR("NIL");
//     default:
//         interpreter->error = PS_ERROR_UNEXPECTED_TOKEN;
//         TRACE_ERROR("?");
//     }

//     VISIT_END("OK");
// }

// /**
//  * Parse
//  *      term                    =   factor [ multiplicative_operator , factor ]* ;
//  *      multiplicative_operator =   '*' | '/' | 'DIV' | 'MOD' | 'AND' | 'SHL' | 'SHR' | 'AS'
//  */
// bool ps_parse_term(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
// {
//     static ps_token_type multiplicative_operators[] = {
//         PS_TOKEN_STAR, PS_TOKEN_SLASH, PS_TOKEN_DIV, PS_TOKEN_MOD,
//         // PS_TOKEN_AND,  PS_TOKEN_SHL,   PS_TOKEN_SHR
//     };

//     VISIT_BEGIN("TERM", "");

//     ps_value left = {.type = ps_system_none.value->data.t, .data.v = NULL},
//              right = {.type = ps_system_none.value->data.t, .data.v = NULL};
//     ps_token_type multiplicative_operator = PS_TOKEN_NONE;
//     if (!ps_parse_factor(interpreter, mode, &left))
//         TRACE_ERROR("FACTOR");
//     do
//     {
//         multiplicative_operator = ps_parser_expect_token_types(
//             interpreter->parser, sizeof(multiplicative_operators) / sizeof(ps_token_type), multiplicative_operators);
//         if (multiplicative_operator == PS_TOKEN_NONE)
//         {
//             if (mode == MODE_EXEC)
//             {
//                 result->type = left.type;
//                 result->data = left.data;
//             }
//             VISIT_END("1");
//         }
//         READ_NEXT_TOKEN;
//         if (!ps_parse_factor(interpreter, mode, &right))
//             TRACE_ERROR("FACTOR");
//         if (mode == MODE_EXEC)
//         {
//             if (!ps_function_binary_op(interpreter, &left, &right, result, multiplicative_operator))
//                 TRACE_ERROR("BINARY");
//             left.type = result->type;
//             left.data = result->data;
//         }
//     } while (true);

//     VISIT_END("2");
// }

// /**
//  * Parse
//  *      simple_expression       =   term [ additive_operator , term ]* ;
//  *      additive_operator       =   '+' | '-'
//  *                                  //  | 'OR' | 'XOR' ;
//  */
// bool ps_parse_simple_expression(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
// {
//     static ps_token_type additive_operators[] = {
//         PS_TOKEN_PLUS, PS_TOKEN_MINUS,
//         // PS_TOKEN_OR, PS_TOKEN_XOR
//     };

//     VISIT_BEGIN("SIMPLE_EXPRESSION", "");
//     ps_value left = {.type = ps_system_none.value->data.t, .data.v = NULL},
//              right = {.type = ps_system_none.value->data.t, .data.v = NULL};
//     ps_token_type additive_operator = PS_TOKEN_NONE;
//     if (!ps_parse_term(interpreter, mode, &left))
//         TRACE_ERROR("TERM");
//     do
//     {
//         additive_operator = ps_parser_expect_token_types(
//             interpreter->parser, sizeof(additive_operators) / sizeof(ps_token_type), additive_operators);
//         if (additive_operator == PS_TOKEN_NONE)
//         {
//             if (mode == MODE_EXEC)
//             {
//                 result->type = left.type;
//                 result->data = left.data;
//             }
//             VISIT_END("1");
//         }
//         READ_NEXT_TOKEN;
//         if (!ps_parse_term(interpreter, mode, &right))
//             TRACE_ERROR("TERM");
//         if (mode == MODE_EXEC)
//         {
//             if (!ps_function_binary_op(interpreter, &left, &right, result, additive_operator))
//                 TRACE_ERROR("BINARY");
//             left.type = result->type;
//             left.data = result->data;
//         }
//     } while (true);
//     VISIT_END("2");
// }

// /**
//  * Parse
//  *      expression              =   simple_expression [ relational_operator , simple_expression ] ;
//  *      relational_operator     =   '<' | '<=' | '>' | '>=' | '=' | '<>' ;
//  */
// bool ps_parse_relational_expression(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
// {
//     static ps_token_type relational_operators[] = {
//         PS_TOKEN_LESS_THAN,        PS_TOKEN_LESS_OR_EQUAL, PS_TOKEN_GREATER_THAN,
//         PS_TOKEN_GREATER_OR_EQUAL, PS_TOKEN_EQUAL,         PS_TOKEN_NOT_EQUAL,
//     };

//     VISIT_BEGIN("RELATIONAL_EXPRESSION", "");
//     ps_value left = {.type = ps_system_none.value->data.t, .data.v = NULL};
//     ps_value right = {.type = ps_system_none.value->data.t, .data.v = NULL};
//     ps_token_type relational_operator = PS_TOKEN_NONE;

//     if (!ps_parse_simple_expression(interpreter, mode, &left))
//         TRACE_ERROR("SIMPLE_LEFT");
//     relational_operator = ps_parser_expect_token_types(
//         interpreter->parser, sizeof(relational_operators) / sizeof(ps_token_type), relational_operators);
//     if (relational_operator == PS_TOKEN_NONE)
//     {
//         if (mode == MODE_EXEC)
//         {
//             result->type = left.type;
//             result->data = left.data;
//         }
//         VISIT_END("LEFT");
//     }
//     READ_NEXT_TOKEN;
//     if (!ps_parse_simple_expression(interpreter, mode, &right))
//         TRACE_ERROR("SIMPLE_RIGHT");
//     if (mode == MODE_EXEC)
//     {
//         result->type = ps_system_boolean.value->data.t;
//         if (!ps_function_binary_op(interpreter, &left, &right, result, relational_operator))
//             TRACE_ERROR("BINARY");
//     }
//     VISIT_END("RIGHT");
// }

// /**
//  * Parse
//  *      and_expression = relational_expression { 'AND' relational_expression }
//  */
// bool ps_parse_and_expression(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
// {
//     static ps_token_type and_operators[] = {PS_TOKEN_AND};

//     VISIT_BEGIN("AND_EXPRESSION", "");
//     ps_value left = {.type = ps_system_none.value->data.t, .data.v = NULL},
//              right = {.type = ps_system_none.value->data.t, .data.v = NULL};
//     ps_token_type and_operator = PS_TOKEN_NONE;
//     if (!ps_parse_relational_expression(interpreter, mode, &left))
//         TRACE_ERROR("RELATIONAL1");
//     do
//     {
//         and_operator = ps_parser_expect_token_types(interpreter->parser, sizeof(and_operators) / sizeof(ps_token_type),
//                                                     and_operators);
//         if (and_operator == PS_TOKEN_NONE)
//         {
//             if (mode == MODE_EXEC)
//             {
//                 result->type = left.type;
//                 result->data = left.data;
//             }
//             VISIT_END("AND1");
//         }
//         READ_NEXT_TOKEN;
//         if (!ps_parse_relational_expression(interpreter, mode, &right))
//             TRACE_ERROR("RELATIONAL2");
//         if (mode == MODE_EXEC)
//         {
//             if (!ps_function_binary_op(interpreter, &left, &right, result, and_operator))
//                 TRACE_ERROR("BINARY");
//             left.type = result->type;
//             left.data = result->data;
//         }
//     } while (true);
//     VISIT_END("AND2");
// }

// /**
//  * Parse
//  *      logical_expression = relational_expression { logical_operator relational_expression }
//  *      logical_operator   = 'AND' | 'OR' | 'XOR'
//  */
// bool ps_parse_logical_expression(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
// {
//     static ps_token_type logical_operators[] = {PS_TOKEN_AND, PS_TOKEN_OR, PS_TOKEN_XOR};

//     VISIT_BEGIN("LOGICAL_EXPRESSION", "");
//     ps_value left = {.type = ps_system_none.value->data.t, .data.v = NULL},
//              right = {.type = ps_system_none.value->data.t, .data.v = NULL};
//     ps_token_type logical_operator = PS_TOKEN_NONE;
//     if (!ps_parse_relational_expression(interpreter, mode, &left))
//         TRACE_ERROR("RELATIONAL");
//     do
//     {
//         logical_operator = ps_parser_expect_token_types(
//             interpreter->parser, sizeof(logical_operators) / sizeof(ps_token_type), logical_operators);
//         if (logical_operator == PS_TOKEN_NONE)
//         {
//             if (mode == MODE_EXEC)
//             {
//                 result->type = left.type;
//                 result->data = left.data;
//             }
//             VISIT_END("LEFT");
//         }
//         READ_NEXT_TOKEN;
//         if (!ps_parse_relational_expression(interpreter, mode, &right))
//             TRACE_ERROR("RELATIONAL2");
//         if (mode == MODE_EXEC)
//         {
//             if (!ps_function_binary_op(interpreter, &left, &right, result, logical_operator))
//                 TRACE_ERROR("BINARY");
//             left.type = result->type;
//             left.data = result->data;
//         }
//     } while (true);
//     VISIT_END("RIGHT");
// }

// /**
//  * Parse
//  *      or_expression = and_expression { ( 'OR' | 'XOR' ) and_expression }
//  */
// bool ps_parse_or_expression(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
// {
//     static ps_token_type or_operators[] = {PS_TOKEN_OR, PS_TOKEN_XOR};

//     VISIT_BEGIN("OR_EXPRESSION", "");
//     ps_value left = {.type = ps_system_none.value->data.t, .data.v = NULL},
//              right = {.type = ps_system_none.value->data.t, .data.v = NULL};
//     ps_token_type or_operator = PS_TOKEN_NONE;
//     if (!ps_parse_and_expression(interpreter, mode, &left))
//         TRACE_ERROR("AND");
//     do
//     {
//         or_operator =
//             ps_parser_expect_token_types(interpreter->parser, sizeof(or_operators) / sizeof(ps_token_type), or_operators);
//         if (or_operator == PS_TOKEN_NONE)
//         {
//             if (mode == MODE_EXEC)
//             {
//                 result->type = left.type;
//                 result->data = left.data;
//             }
//             VISIT_END("LEFT");
//         }
//         READ_NEXT_TOKEN;
//         if (!ps_parse_and_expression(interpreter, mode, &right))
//             TRACE_ERROR("AND2");
//         if (mode == MODE_EXEC)
//         {
//             if (!ps_function_binary_op(interpreter, &left, &right, result, or_operator))
//                 TRACE_ERROR("BINARY");
//             left.type = result->type;
//             left.data = result->data;
//         }
//     } while (true);
//     VISIT_END("RIGHT");
// }

// bool ps_parse_expression(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_value *result)
// {
//     return ps_parse_or_expression(interpreter, mode, result);
// }

// /**
//  * Parse CONST IDENTIFIER = VALUE;
//  *             IDENTIFIER = VALUE;
//  *             ...
//  * Next steps:
//  *       IDENTIFIER = IDENTIFIER | VALUE ;
//  *       IDENTIFIER = CONSTANT_EXPRESSION ;
//  */
// bool ps_parse_const(ps_interpreter *interpreter, ps_interpreter_mode mode)
// {
//     VISIT_BEGIN("CONST", "");
//     ps_identifier identifier;
//     ps_type_definition *type;
//     ps_value *value;
//     ps_value_data data;
//     ps_symbol *constant;
//     bool negate = false;

//     EXPECT_TOKEN(PS_TOKEN_CONST);
//     READ_NEXT_TOKEN;
//     do
//     {
//         EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
//         COPY_IDENTIFIER(identifier);
//         READ_NEXT_TOKEN;
//         EXPECT_TOKEN(PS_TOKEN_EQUAL);
//         READ_NEXT_TOKEN;
//         // TODO allow constant expression
//         // For now keep track of '-' so "Const Foo = -4;" works as expected
//         if (lexer->current_token.type == PS_TOKEN_MINUS)
//         {
//             negate = true;
//             READ_NEXT_TOKEN;
//         }
//         else
//         {
//             negate = false;
//         }
//         switch (lexer->current_token.type)
//         {
//         case PS_TOKEN_IDENTIFIER:
//             constant = ps_interpreter_find_symbol(interpreter, &lexer->current_token.value.identifier, true);
//             if (constant == NULL)
//                 RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
//             if (constant->kind != PS_SYMBOL_KIND_CONSTANT)
//                 RETURN_ERROR(PS_ERROR_EXPECTED_CONSTANT);
//             type = constant->value->type;
//             data = constant->value->data;
//             break;
//         case PS_TOKEN_INTEGER_VALUE:
//             type = ps_system_integer.value->data.t;
//             data.i = negate ? -lexer->current_token.value.i : lexer->current_token.value.i;
//             break;
//         case PS_TOKEN_REAL_VALUE:
//             type = ps_system_real.value->data.t;
//             data.r = negate ? -lexer->current_token.value.r : lexer->current_token.value.r;
//             break;
//         case PS_TOKEN_UNSIGNED_VALUE:
//             if (negate)
//             {
//                 type = ps_system_integer.value->data.t;
//                 data.i = -lexer->current_token.value.i;
//             }
//             else
//             {
//                 type = ps_system_unsigned.value->data.t;
//                 data.u = lexer->current_token.value.u;
//             }
//             break;
//         case PS_TOKEN_CHAR_VALUE:
//             if (negate)
//                 RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
//             type = ps_system_char.value->data.t;
//             data.c = lexer->current_token.value.c;
//             break;
//         case PS_TOKEN_BOOLEAN_VALUE:
//             if (negate)
//                 RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
//             type = ps_system_boolean.value->data.t;
//             data.b = lexer->current_token.value.b;
//             break;
//         case PS_TOKEN_STRING_VALUE:
//             ps_string *s = ps_string_heap_create(interpreter->string_heap, lexer->current_token.value.s);
//             if (s == NULL)
//                 RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
//             type = ps_system_string.value->data.t;
//             data.s = s;
//             break;

//         default:
//             RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
//         }
//         READ_NEXT_TOKEN;
//         EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
//         READ_NEXT_TOKEN;
//         if (mode == MODE_EXEC)
//         {
//             value = ps_value_alloc(type, data);
//             if (value == NULL)
//                 RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
//             constant = ps_symbol_alloc(PS_SYMBOL_KIND_CONSTANT, &identifier, value);
//             if (constant == NULL)
//                 RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
//             if (!ps_interpreter_add_symbol(interpreter, constant))
//                 RETURN_ERROR(interpreter->error);
//         }
//     } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);

//     VISIT_END("OK");
// }

// // /**
// //  * Parse
// //  *      TYPE IDENTIFIER '=' INTEGER | UNSIGNED | REAL | BOOLEAN | CHAR | STRING ';'
// //  *           ...
// //  * Next steps:
// //  *      TYPE IDENTIFIER = TYPE_DEFINITION ;
// //  *      TYPE_DEFINITION = SUBRANGE | ENUMERATION | POINTER | RECORD | ARRAY | FILE | STRING ;
// //  *       SUBRANGE = INTEGER | UNSIGNED | IDENTIFIER '..' INTEGER | UNSIGNED | IDENTIFIER ;
// //  */
// // bool ps_parse_type(ps_interpreter *interpreter, ps_interpreter_mode mode)
// // {
// //     VISIT_BEGIN("TYPE", "");
// //     ps_identifier identifier = {0};
// //     ps_type_definition *type = NULL;
// //     ps_value *value = NULL;
// //     ps_value_data data = {0};
// //     ps_symbol *symbol = NULL;
// //     ps_unsigned len = 0;

// //     EXPECT_TOKEN(PS_TOKEN_TYPE);
// //     READ_NEXT_TOKEN;
// //     do
// //     {
// //         EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
// //         COPY_IDENTIFIER(identifier);
// //         READ_NEXT_TOKEN;
// //         EXPECT_TOKEN(PS_TOKEN_EQUAL);
// //         READ_NEXT_TOKEN;
// //         switch (lexer->current_token.type)
// //         {
// //         case PS_TOKEN_INTEGER:
// //             type = ps_system_integer.value->data.t;
// //             break;
// //         case PS_TOKEN_UNSIGNED:
// //             type = ps_system_unsigned.value->data.t;
// //             break;
// //         case PS_TOKEN_REAL:
// //             type = ps_system_real.value->data.t;
// //             break;
// //         case PS_TOKEN_BOOLEAN:
// //             type = ps_system_boolean.value->data.t;
// //             break;
// //         case PS_TOKEN_CHAR:
// //             type = ps_system_char.value->data.t;
// //             break;
// //         case PS_TOKEN_STRING:
// //             type = ps_system_string.value->data.t;
// //             // if (lexer->current_token.type == PS_TOKEN_LEFT_BRACKET)
// //             // {
// //             //     READ_NEXT_TOKEN;
// //             //     if (lexer->current_token.type == PS_TOKEN_UNSIGNED_VALUE)
// //             //     {
// //             //         len = lexer->current_token.value.u;
// //             //     }
// //             //     else if (lexer->current_token.type == PS_TOKEN_IDENTIFIER)
// //             //     {
// //             //         symbol = ps_interpreter_find_symbol(interpreter, &lexer->current_token.value.identifier);
// //             //         if (symbol == NULL)
// //             //             RETURN_ERROR(PS_ERROR_UNKOWN_IDENTIFIER);
// //             //         if (symbol->kind != PS_SYMBOL_KIND_CONSTANT ||
// //             //             symbol->value->type != ps_system_unsigned.value->data.t)
// //             //             RETURN_ERROR(PS_ERROR_EXPECTED_UNSIGNED);
// //             //         len = (ps_unsigned)lexer->current_token.value.i;
// //             //     }
// //             //     else
// //             //         RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
// //             //     if (len < 1 || len > PS_STRING_MAX_LEN)
// //             //         RETURN_ERROR(PS_ERROR_EXPECTED_STRING_LENGTH);
// //             // }
// //             break;
// //         // case PS_TOKEN_ARRAY:
// //         //     type = ps_system_array.value->data.t; // TODO: parse array definition
// //         //     data.s = NULL; // TODO: allocate array
// //         //     break;
// //         case PS_TOKEN_IDENTIFIER:
// //             RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
// //         default:
// //             RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
// //         }
// //         READ_NEXT_TOKEN;
// //         EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
// //         READ_NEXT_TOKEN;

// //         if (mode==MODE_EXEC)
// //         {
// //             value = ps_value_alloc(type, data);
// //             if (value == NULL)
// //                 RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
// //             symbol = ps_symbol_alloc(PS_SYMBOL_KIND_TYPE_DEFINITION, &identifier, value);
// //             if (symbol == NULL)
// //                 RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
// //             if (!ps_interpreter_add_symbol(interpreter, symbol))
// //                 RETURN_ERROR(interpreter->error);
// //         }
// //     } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);

// //     VISIT_END("OK");
// // }

// /**
//  * Parse    VAR IDENTIFIER : TYPE;
//  *              IDENTIFIER : TYPE;
//  *          ...
//  * Next step: allow identifier list with commas
//  *              IDENTIFIER, IDENTIFIER, ... : TYPE ;
//  */
// bool ps_parse_var(ps_interpreter *interpreter, ps_interpreter_mode mode)
// {
//     VISIT_BEGIN("VAR", "");
//     ps_identifier identifier[8];
//     int var_count;
//     ps_type_definition *type;
//     ps_value *value;
//     ps_value_data data;
//     ps_symbol *variable;
//     EXPECT_TOKEN(PS_TOKEN_VAR);
//     READ_NEXT_TOKEN;
//     do
//     {
//         var_count = 0;
//         do
//         {
//             EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
//             COPY_IDENTIFIER(identifier[var_count]);
//             READ_NEXT_TOKEN;
//             if (lexer->current_token.type == PS_TOKEN_COLON)
//                 break;
//             if (lexer->current_token.type == PS_TOKEN_COMMA)
//             {
//                 READ_NEXT_TOKEN;
//                 var_count++;
//                 if (var_count > 8 - 1)
//                     RETURN_ERROR(PS_ERROR_TOO_MANY_VARIABLES);
//                 continue;
//             }
//         } while (true);
//         EXPECT_TOKEN(PS_TOKEN_COLON);
//         READ_NEXT_TOKEN;
//         switch (lexer->current_token.type)
//         {
//         case PS_TOKEN_BOOLEAN:
//             type = ps_system_boolean.value->data.t;
//             data.b = (ps_boolean) false;
//             break;
//         case PS_TOKEN_CHAR:
//             type = ps_system_char.value->data.t;
//             data.c = '\0';
//             break;
//         case PS_TOKEN_INTEGER:
//             type = ps_system_integer.value->data.t;
//             data.i = 0;
//             break;
//         case PS_TOKEN_UNSIGNED:
//             type = ps_system_unsigned.value->data.t;
//             data.u = 0;
//             break;
//         case PS_TOKEN_REAL:
//             type = ps_system_real.value->data.t;
//             data.r = 0.0;
//             break;
//         case PS_TOKEN_STRING:
//             type = ps_system_string.value->data.t;
//             data.s = NULL;
//             break;
//         // case PS_TOKEN_ARRAY:
//         //     type = ps_system_array.value->data.t;
//         //     data.s = NULL;
//         //     break;
//         case PS_TOKEN_IDENTIFIER:
//             RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
//         default:
//             RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
//         }
//         READ_NEXT_TOKEN;
//         EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
//         READ_NEXT_TOKEN;
//         // if (mode==MODE_EXEC)
//         // {
//         for (int i = 0; i <= var_count; i++)
//         {
//             value = ps_value_alloc(type, data);
//             variable = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, &identifier[i], value);
//             if (variable == NULL)
//                 RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY);
//             if (!ps_interpreter_add_symbol(interpreter, variable))
//                 RETURN_ERROR(interpreter->error);
//         }
//         // }
//     } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);
//     VISIT_END("OK");
// }

// /**
//  * Parse IDENTIFIER := EXPRESSION
//  */
// bool ps_parse_assignment(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_identifier *identifier)
// {
//     VISIT_BEGIN("ASSIGNMENT", "");
//     ps_symbol *variable;
//     ps_value result = {.type = ps_system_none.value->data.t, .data.v = NULL};

//     EXPECT_TOKEN(PS_TOKEN_ASSIGN);
//     READ_NEXT_TOKEN;

//     if (mode == MODE_EXEC)
//     {
//         variable = ps_interpreter_find_symbol(interpreter, identifier, false);
//         if (variable == NULL)
//         {
//             interpreter->error = PS_ERROR_SYMBOL_NOT_FOUND;
//             TRACE_ERROR("VARIABLE1");
//         }
//         if (variable->kind == PS_SYMBOL_KIND_CONSTANT)
//         {
//             interpreter->error = PS_ERROR_ASSIGN_TO_CONST;
//             TRACE_ERROR("CONST");
//         }
//         if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
//         {
//             interpreter->error = PS_ERROR_EXPECTED_VARIABLE;
//             TRACE_ERROR("VARIABLE2");
//         }
//         result.type = variable->value->type;
//         if (!ps_parse_expression(interpreter, mode, &result))
//             TRACE_ERROR("EXPRESSION1");
//         if (interpreter->debug)
//             ps_value_debug(stderr, "ASSIGN => ", &result);
//         if (!ps_interpreter_copy_value(interpreter, &result, variable->value))
//             TRACE_ERROR("COPY");
//     }
//     else if (!ps_parse_expression(interpreter, MODE_SKIP, &result))
//         TRACE_ERROR("EXPRESSION2");

//     VISIT_END("OK");
// }

// /**
//  * Parse
//  *      write_or_writeln        =   ( 'WRITE' | 'WRITELN' ) [ '(' expression [ ',' expression ]* ')' ] ;
//  * Next step:
//  *      write_or_writeln        =   ( 'WRITE' | 'WRITELN' ) [ '('
//  *                                            expression [ ':' width [ ':' precision ] ]
//  *                                      [ ',' expression [ ':' width [ ':' precision ] ] ]*
//  *                                    ')' ] ;
//  */
// bool ps_parse_write_or_writeln(ps_interpreter *interpreter, ps_interpreter_mode mode, bool newline)
// {
//     VISIT_BEGIN("WRITE_OR_WRITELN", "");
//     ps_value result = {.type = ps_system_none.value->data.t, .data.v = NULL};
//     bool loop = true;

//     // "Write[Ln];" or "Write[Ln] Else|End|Until"?
//     if (lexer->current_token.type == PS_TOKEN_SEMI_COLON || lexer->current_token.type == PS_TOKEN_ELSE ||
//         lexer->current_token.type == PS_TOKEN_END || lexer->current_token.type == PS_TOKEN_UNTIL)
//     {
//         if (mode == MODE_EXEC && newline)
//             fprintf(stdout, "\n");
//         VISIT_END("EMPTY1");
//     }
//     EXPECT_TOKEN(PS_TOKEN_LEFT_PARENTHESIS);
//     READ_NEXT_TOKEN;
//     // "Write[Ln]()"?
//     if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
//     {
//         if (mode == MODE_EXEC && newline)
//             fprintf(stdout, "\n");
//         READ_NEXT_TOKEN;
//         loop = false;
//     }

//     while (loop)
//     {
//         if (!ps_parse_expression(interpreter, mode, &result))
//             TRACE_ERROR("EXPR");
//         if (mode == MODE_EXEC)
//         {
//             if (!ps_procedure_write(interpreter, stdout, &result))
//                 TRACE_ERROR("WRITE");
//         }
//         if (lexer->current_token.type == PS_TOKEN_COMMA)
//         {
//             READ_NEXT_TOKEN;
//             continue;
//         }
//         EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
//         READ_NEXT_TOKEN;
//         loop = false;
//     }

//     if (mode == MODE_EXEC && newline)
//         fprintf(stdout, "\n");

//     VISIT_END("OK");
// }

// bool ps_parse_procedure_call(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol *executable, uint16_t line,
//                              uint8_t column)
// {
//     VISIT_BEGIN("PROCEDURE_CALL", "");
//     bool has_environment = false;

//     if (executable == &ps_system_procedure_write || executable == &ps_system_procedure_writeln)
//     {
//         // Write or Writeln
//         if (!ps_parse_write_or_writeln(interpreter, mode, executable == &ps_system_procedure_writeln))
//             TRACE_ERROR("WRITE!");
//     }
//     else if (executable == &ps_system_procedure_randomize)
//     {
//         // Randomize
//         if (mode == MODE_EXEC)
//             if (!ps_procedure_randomize(interpreter, NULL))
//                 TRACE_ERROR("RANDOMIZE!");
//     }
//     else
//     {
//         // User defined procedure call
//         // Enter environment for procedure call
//         if (!ps_interpreter_enter_environment(interpreter, &executable->name))
//             RETURN_ERROR(interpreter->error);
//         has_environment = true;
//         // TODO Parse parameters (needs environment)
//         // for now, just check for statement terminators
//         ps_token_type token_type = ps_parser_expect_token_types(
//             interpreter->parser, 4, (ps_token_type[]){PS_TOKEN_SEMI_COLON, PS_TOKEN_END, PS_TOKEN_ELSE, PS_TOKEN_UNTIL});
//         if (token_type == PS_TOKEN_NONE)
//         {
//             interpreter->error = PS_ERROR_UNEXPECTED_TOKEN;
//             goto cleanup;
//         }
//         // READ_NEXT_TOKEN;
//         // Execute procedure
//         if (mode == MODE_EXEC)
//         {
//             fprintf(stderr, "================================================================================\n");
//             ps_token_debug(stderr, "CURRENT", &lexer->current_token);
//             // Set cursor to the beginning of the procedure body
//             if (!ps_lexer_set_cursor(lexer, executable->value->data.x->line, executable->value->data.x->column))
//             {
//                 interpreter->error = PS_ERROR_GENERIC; // TODO better error code
//                 goto cleanup;
//             }
//             READ_NEXT_TOKEN;
//             fprintf(stderr, "================================================================================\n");
//             // Parse procedure body
//             if (!ps_parse_block(interpreter, mode))
//                 goto cleanup;
//             // Restore cursor position
//             if (!ps_lexer_set_cursor(lexer, line, column))
//             {
//                 interpreter->error = PS_ERROR_GENERIC; // TODO better error code
//                 goto cleanup;
//             }
//             READ_NEXT_TOKEN;
//         }
//         // Exit environment
//         if (!ps_interpreter_exit_environment(interpreter))
//             RETURN_ERROR(interpreter->error);
//     }

//     VISIT_END("OK");

// cleanup:
//     if (has_environment)
//         ps_interpreter_exit_environment(interpreter);
//     if (interpreter->error == PS_ERROR_NONE)
//         interpreter->error = PS_ERROR_GENERIC;
//     TRACE_ERROR("CLEANUP");
// }

// bool ps_parse_assignment_or_procedure_call(ps_interpreter *interpreter, ps_interpreter_mode mode)
// {
//     VISIT_BEGIN("ASSIGNMENT_OR_PROCEDURE_CALL", "");
//     uint16_t line = 0;
//     uint8_t column = 0;
//     ps_identifier identifier;
//     ps_symbol *symbol;

//     TRACE_CURSOR;
//     // Save "cursor" position
//     if (!ps_lexer_get_cursor(lexer, &line, &column))
//         RETURN_ERROR(PS_ERROR_GENERIC); // TODO better error code
//     COPY_IDENTIFIER(identifier);
//     READ_NEXT_TOKEN;
//     symbol = ps_interpreter_find_symbol(interpreter, &identifier, false);
//     if (symbol == NULL)
//         RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
//     switch (symbol->kind)
//     {
//     case PS_SYMBOL_KIND_VARIABLE:
//         if (!ps_parse_assignment(interpreter, mode, &identifier))
//             TRACE_ERROR("ASSIGN!");
//         break;
//     case PS_SYMBOL_KIND_CONSTANT:
//         RETURN_ERROR(PS_ERROR_ASSIGN_TO_CONST);
//     case PS_SYMBOL_KIND_PROCEDURE:
//         if (!ps_parse_procedure_call(interpreter, mode, symbol, line, column))
//             RETURN_ERROR(interpreter->error);
//         break;
//     default:
//         RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
//     }

//     VISIT_END("OK");
// }

// /**
//  * Parse BEGIN
//  *         [ STATEMENT ... ] [ ; ]
//  *       END
//  * NB: ';' or '.' or whatever after END is analyzed in the caller
//  */
// bool ps_parse_compound_statement(ps_interpreter *interpreter, ps_interpreter_mode mode)
// {
//     VISIT_BEGIN("COMPOUND_STATEMENT", "");

//     EXPECT_TOKEN(PS_TOKEN_BEGIN);
//     READ_NEXT_TOKEN;
//     if (lexer->current_token.type != PS_TOKEN_END)
//     {
//         if (!ps_parse_statement_list(interpreter, mode, PS_TOKEN_END))
//             TRACE_ERROR("STATEMENTS");
//     }
//     EXPECT_TOKEN(PS_TOKEN_END);
//     READ_NEXT_TOKEN;

//     VISIT_END("OK");
// }

// /**
//  * Parse
//  *      if_statement = 'IF' expression 'THEN' statement [ 'ELSE' statement ] ;
//  */
// bool ps_parse_if_then_else(ps_interpreter *interpreter, ps_interpreter_mode mode)
// {
//     VISIT_BEGIN("IF_THEN_ELSE", "");
//     ps_value result = {.type = ps_system_boolean.value->data.t, .data.b = false};

//     READ_NEXT_TOKEN;
//     if (!ps_parse_expression(interpreter, mode, &result))
//         TRACE_ERROR("TEST");
//     if (result.type != ps_system_boolean.value->data.t)
//         RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
//     EXPECT_TOKEN(PS_TOKEN_THEN);
//     READ_NEXT_TOKEN;
//     if (!ps_parse_statement(interpreter, mode && result.data.b))
//         TRACE_ERROR("THEN");
//     if (lexer->current_token.type == PS_TOKEN_ELSE)
//     {
//         READ_NEXT_TOKEN;
//         if (!ps_parse_statement(interpreter, mode && !result.data.b))
//             TRACE_ERROR("ELSE");
//     }

//     VISIT_END("OK");
// }

// /**
//  * Parse
//  *      repeat_statement = 'REPEAT' statement_list [ ';' ] 'UNTIL' expression ;
//  */
// bool ps_parse_repeat_until(ps_interpreter *interpreter, ps_interpreter_mode mode)
// {
//     VISIT_BEGIN("REPEAT_UNTIL", "");
//     ps_value result = {.type = ps_system_boolean.value->data.t, .data.b = false};
//     uint16_t line = 0;
//     uint8_t column = 0;

//     ps_lexer_get_cursor(lexer, &line, &column);
//     READ_NEXT_TOKEN;
//     do
//     {
//         if (!ps_parse_statement_list(interpreter, mode, PS_TOKEN_UNTIL))
//             TRACE_ERROR("STATEMENTS");
//         // Skip optional ';'
//         if (lexer->current_token.type == PS_TOKEN_SEMI_COLON)
//             READ_NEXT_TOKEN;
//         EXPECT_TOKEN(PS_TOKEN_UNTIL);
//         READ_NEXT_TOKEN;
//         if (!ps_parse_expression(interpreter, mode, &result))
//             TRACE_ERROR("EXPRESSION");
//         if (result.type != ps_system_boolean.value->data.t)
//             RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
//         if (mode != MODE_EXEC || result.data.b)
//             break;
//         if (!ps_lexer_set_cursor(lexer, line, column))
//             RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE); // TODO better error code
//         READ_NEXT_TOKEN;
//     } while (true);

//     VISIT_END("OK");
// }

// /**
//  * Parse
//  *      'WHILE' expression 'DO' statement
//  */
// bool ps_parse_while_do(ps_interpreter *interpreter, ps_interpreter_mode mode)
// {
//     VISIT_BEGIN("WHILE_DO", "");
//     ps_value result = {.type = ps_system_boolean.value->data.t, .data.b = false};
//     uint16_t line = 0;
//     uint8_t column = 0;

//     // Save "cursor" position
//     if (!ps_lexer_get_cursor(lexer, &line, &column))
//         TRACE_ERROR("CURSOR!");
//     READ_NEXT_TOKEN;
//     do
//     {
//         if (!ps_parse_expression(interpreter, mode, &result))
//             TRACE_ERROR("EXPRESSION");
//         if (result.type != ps_system_boolean.value->data.t)
//             RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
//         EXPECT_TOKEN(PS_TOKEN_DO);
//         READ_NEXT_TOKEN;
//         if (!ps_parse_statement(interpreter, mode && result.data.b))
//             TRACE_ERROR("STATEMENT");
//         if (mode != MODE_EXEC || !result.data.b)
//             break;
//         if (!ps_lexer_set_cursor(lexer, line, column))
//             RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE); // TODO better error code
//         READ_NEXT_TOKEN;
//     } while (true);

//     VISIT_END("OK");
// }

// /**
//  * Parse
//  *      for_statement = 'FOR' control_variable ':=' expression ( 'TO' |
//  * 'DOWNTO' ) expression 'DO' statement ;
//  */
// bool ps_parse_for_do(ps_interpreter *interpreter, ps_interpreter_mode mode)
// {
//     VISIT_BEGIN("FOR_DO", "");
//     ps_value start = {.type = ps_system_none.value->data.t, .data.v = NULL};
//     ps_value finish = {.type = ps_system_none.value->data.t, .data.v = NULL};
//     ps_value step = {.type = ps_system_none.value->data.t, .data.v = NULL};
//     ps_value result = {.type = ps_system_boolean.value->data.t, .data.b = false};
//     ps_identifier identifier;
//     ps_symbol *variable;
//     uint16_t line = 0;
//     uint8_t column = 0;

//     // FOR
//     READ_NEXT_TOKEN;
//     // IDENTIFIER
//     EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
//     if (mode == MODE_EXEC)
//     {
//         COPY_IDENTIFIER(identifier);
//         variable = ps_interpreter_find_symbol(interpreter, &identifier, true);
//         if (variable == NULL)
//             RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
//         if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
//             RETURN_ERROR(PS_ERROR_EXPECTED_VARIABLE);
//         start.type = variable->value->type;
//         finish.type = variable->value->type;
//     }
//     // :=
//     READ_NEXT_TOKEN;
//     EXPECT_TOKEN(PS_TOKEN_ASSIGN);
//     // START VALUE
//     READ_NEXT_TOKEN;
//     if (!ps_parse_expression(interpreter, mode, &start))
//         TRACE_ERROR("START");
//     // TO | DOWNTO
//     if (lexer->current_token.type == PS_TOKEN_TO)
//         step.data.i = 1;
//     else if (lexer->current_token.type == PS_TOKEN_DOWNTO)
//         step.data.i = -1;
//     else
//         RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
//     READ_NEXT_TOKEN;
//     // FINISH VALUE
//     if (!ps_parse_expression(interpreter, mode, &finish))
//         TRACE_ERROR("FINISH");
//     // DO
//     EXPECT_TOKEN(PS_TOKEN_DO);
//     ps_lexer_get_cursor(lexer, &line, &column);
//     READ_NEXT_TOKEN;
//     if (mode != MODE_EXEC)
//     {
//         if (!ps_parse_statement_or_compound_statement(interpreter, MODE_SKIP))
//             TRACE_ERROR("STATEMENT_OR_COMPOUND");
//     }
//     else
//     {
//         // VARIABLE := START
//         if (!ps_interpreter_copy_value(interpreter, &start, variable->value))
//             TRACE_ERROR("COPY");
//         // Loop while variable <= finish for "TO"
//         // (or variable >= finish for "DOWNTO")
//         do
//         {
//             if (!ps_function_binary_op(interpreter, variable->value, &finish, &result,
//                                        step.data.i > 0 ? PS_TOKEN_LESS_OR_EQUAL : PS_TOKEN_GREATER_OR_EQUAL))
//                 TRACE_ERROR("BINARY");
//             if (result.type != ps_system_boolean.value->data.t)
//                 RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
//             if (!result.data.b)
//             {
//                 // End of loop => skip statement
//                 if (!ps_parse_statement_or_compound_statement(interpreter, MODE_SKIP))
//                     TRACE_ERROR("BODY");
//                 break;
//             }
//             if (!ps_parse_statement_or_compound_statement(interpreter, mode))
//                 TRACE_ERROR("BODY");
//             if (!ps_lexer_set_cursor(lexer, line, column))
//                 RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE); // TODO better error code
//             READ_NEXT_TOKEN;
//             // VARIABLE := SUCC/PRED(VARIABLE)
//             if (step.data.i > 0)
//             {
//                 if (!ps_function_succ(interpreter, variable->value, variable->value))
//                     TRACE_ERROR("STEP/SUCC");
//             }
//             else
//             {
//                 if (!ps_function_pred(interpreter, variable->value, variable->value))
//                     TRACE_ERROR("STEP/PRED");
//             }
//         } while (true);
//     }

//     VISIT_END("OK");
// }

// /**
//  * Parse statement
//  *      compound_statement      =   'BEGIN' statement_list [ ';' ] 'END' ;
//  *      statement               =   assignment_statement
//  *                              |   procedure_call
//  *                              |   if_statement
//  *                              |   repeat_statement
//  *                              |   while_statement
//  *                              |   for_statement
//  *                              ;
//  */
// bool ps_parse_statement(ps_interpreter *interpreter, ps_interpreter_mode mode)
// {
//     VISIT_BEGIN("STATEMENT", "");

//     switch (lexer->current_token.type)
//     {
//     case PS_TOKEN_BEGIN:
//         if (!ps_parse_compound_statement(interpreter, mode))
//             TRACE_ERROR("COMPOUND");
//         break;
//     case PS_TOKEN_IDENTIFIER:
//         if (!ps_parse_assignment_or_procedure_call(interpreter, mode))
//             TRACE_ERROR("ASSIGNMENT/PROCEDURE");
//         break;
//     case PS_TOKEN_IF:
//         if (!ps_parse_if_then_else(interpreter, mode))
//             TRACE_ERROR("IF");
//         break;
//     case PS_TOKEN_REPEAT:
//         if (!ps_parse_repeat_until(interpreter, mode))
//             TRACE_ERROR("REPEAT");
//         break;
//     case PS_TOKEN_WHILE:
//         if (!ps_parse_while_do(interpreter, mode))
//             TRACE_ERROR("WHILE");
//         break;
//     case PS_TOKEN_FOR:
//         if (!ps_parse_for_do(interpreter, mode))
//             TRACE_ERROR("FOR");
//         break;
//     default:
//         RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
//     }

//     VISIT_END("OK");
// }

// /**
//  * Parse statement sequence, stopping at "stop" token (e.g. END, ELSE, UNTIL)
//  */
// bool ps_parse_statement_list(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_token_type stop)
// {
//     VISIT_BEGIN("STATEMENT_LIST", "");

//     if (lexer->current_token.type == stop)
//     {
//         READ_NEXT_TOKEN;
//     }
//     else
//     {
//         // let's go!
//         do
//         {
//             if (!ps_parse_statement(interpreter, mode))
//                 TRACE_ERROR("STATEMENT");
//             // NB: semi-colon at statement list end is optional
//             if (lexer->current_token.type == PS_TOKEN_SEMI_COLON)
//             {
//                 READ_NEXT_TOKEN;
//                 if (lexer->current_token.type == stop)
//                     break;
//             }
//             else if (lexer->current_token.type == stop)
//             {
//                 break;
//             }
//             else
//             {
//                 RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
//             }
//         } while (true);
//     }

//     VISIT_END("OK");
// }

// bool ps_parse_statement_or_compound_statement(ps_interpreter *interpreter, ps_interpreter_mode mode)
// {
//     VISIT_BEGIN("STATEMENT_OR_COMPOUND_STATEMENT", "");

//     if (lexer->current_token.type == PS_TOKEN_BEGIN)
//     {
//         if (!ps_parse_compound_statement(interpreter, mode))
//             TRACE_ERROR("COMPOUND");
//     }
//     else
//     {
//         if (!ps_parse_statement(interpreter, mode))
//             TRACE_ERROR("STATEMENT");
//     }

//     VISIT_END("OK");
// }

// /**
//  * Parse
//  *      PROCEDURE IDENTIFIER ;
//  * Next steps:
//  *  - allow procedure block with empty body:
//  *      PROCEDURE IDENTIFIER ;
//  *      BEGIN
//  *      END ;
//  *  - allow procedure block (constants, variables, body):
//  *      PROCEDURE IDENTIFIER
//  *      [ CONST ... TYPE ... VAR ... ]*
//  *      BEGIN
//  *          COMPOUND_STATEMENT [ ; ]
//  *      END ;
//  *  - allow procedure parameters
//  */
// bool ps_parse_procedure_or_function(ps_interpreter *interpreter, ps_interpreter_mode mode, ps_symbol_kind kind)
// {
//     VISIT_BEGIN("PROCEDURE_OR_FUNCTION", "");
//     ps_identifier identifier;
//     ps_symbol *callable = NULL;
//     ps_value *value = NULL;
//     ps_executable *executable = NULL;
//     uint16_t line = 0;
//     uint8_t column = 0;
//     bool has_environment = false;

//     if (kind == PS_SYMBOL_KIND_FUNCTION)
//     {
//         RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
//     }

//     READ_NEXT_TOKEN;
//     EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
//     COPY_IDENTIFIER(identifier);
//     callable = ps_interpreter_find_symbol(interpreter, &identifier, true);
//     if (callable != NULL)
//         RETURN_ERROR(PS_ERROR_SYMBOL_EXISTS);

//     TRACE_CURSOR;
//     if (!ps_lexer_get_cursor(lexer, &line, &column))
//     {
//         interpreter->error = PS_ERROR_GENERIC; // TODO better error code
//         goto cleanup;
//     }
//     READ_NEXT_TOKEN;
//     // NB: no parameters for now
//     EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);

//     if (mode == MODE_EXEC)
//     {
//         executable = calloc(1, sizeof(ps_executable)); // TODO ps_executable_alloc(NULL, NULL, line, column);
//         if (executable == NULL)
//         {
//             interpreter->error = PS_ERROR_OUT_OF_MEMORY;
//             goto cleanup;
//         }
//         executable->signature = NULL;
//         executable->return_type = NULL;
//         executable->line = line;
//         executable->column = column;
//         callable = ps_symbol_alloc(PS_SYMBOL_KIND_PROCEDURE, &identifier, NULL);
//         if (callable == NULL)
//         {
//             interpreter->error = PS_ERROR_OUT_OF_MEMORY;
//             goto cleanup;
//         }
//         callable->kind = kind;
//         value = ps_value_alloc(ps_system_procedure.value->data.t, (ps_value_data){.x = executable});
//         if (value == NULL)
//         {
//             interpreter->error = PS_ERROR_OUT_OF_MEMORY;
//             goto cleanup;
//         }
//         callable->value = value;
//         if (!ps_interpreter_add_symbol(interpreter, callable))
//         {
//             goto cleanup;
//         }
//         if (!ps_interpreter_enter_environment(interpreter, &identifier))
//         {
//             goto cleanup;
//         }
//         has_environment = true;
//     }
//     // Skip block
//     fprintf(stderr, "================================================================================\n");
//     ps_token_debug(stderr, "CURRENT", &lexer->current_token);
//     READ_NEXT_TOKEN;
//     fprintf(stderr, "================================================================================\n");
//     if (!ps_parse_block(interpreter, MODE_SKIP))
//     {
//         goto cleanup;
//     }
//     if (mode == MODE_EXEC)
//     {
//         if (!ps_interpreter_exit_environment(interpreter))
//         {
//             has_environment = false;
//             goto cleanup;
//         }
//     }
//     EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
//     READ_NEXT_TOKEN;

//     VISIT_END("OK");

// cleanup:
//     if (has_environment)
//         ps_interpreter_exit_environment(interpreter);
//     if (callable != NULL)
//         ps_symbol_free(callable);
//     if (value != NULL)
//         ps_value_free(value);
//     if (executable != NULL)
//         free(executable); // TODO ps_executable_free(executable);
//     if (has_environment)
//         ps_interpreter_exit_environment(interpreter);
//     if (interpreter->error == PS_ERROR_NONE)
//         interpreter->error = PS_ERROR_GENERIC;
//     TRACE_ERROR("CLEANUP");
// }

// /**
//  * Parse [ CONST ... TYPE ... VAR ... ]*
//  *       COMPOUND_STATEMENT
//  * NB: ; or . or whatever after END is analyzed in the caller
//  */
// bool ps_parse_block(ps_interpreter *interpreter, ps_interpreter_mode mode)
// {
//     VISIT_BEGIN("BLOCK", "");

//     TRACE_CURSOR;

//     bool loop = true;
//     do
//     {
//         switch (lexer->current_token.type)
//         {
//         case PS_TOKEN_CONST:
//             if (!ps_parse_const(interpreter, mode))
//                 TRACE_ERROR("CONST");
//             break;
//         case PS_TOKEN_TYPE:
//             // if (!ps_parse_type(interpreter, mode))
//             //     TRACE_ERROR("TYPE");
//             // break;
//             RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
//         case PS_TOKEN_VAR:
//             if (!ps_parse_var(interpreter, mode))
//                 TRACE_ERROR("VAR");
//             break;
//         case PS_TOKEN_PROCEDURE:
//             if (!ps_parse_procedure_or_function(interpreter, mode, PS_SYMBOL_KIND_PROCEDURE))
//                 TRACE_ERROR("PROCEDURE");
//             ps_symbol_table_dump(ps_interpreter_get_environment(interpreter)->symbols, "PROCEDURE1?", stderr);
//             break;
//         case PS_TOKEN_FUNCTION:
//             if (!ps_parse_procedure_or_function(interpreter, mode, PS_SYMBOL_KIND_FUNCTION))
//                 TRACE_ERROR("FUNCTION");
//             break;
//         case PS_TOKEN_BEGIN:
//             loop = false;
//             break;
//         default:
//             RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
//         }
//     } while (loop);

//     if (!ps_parse_compound_statement(interpreter, mode))
//         TRACE_ERROR("COMPOUND");

//     VISIT_END("OK");
// }

// /**
//  * Parse
//  *      PROGRAM IDENTIFIER ';'
//  * * Next step:
//  *  - skip optional parameters enclosed in parentheses, like
//  *      PROGRAM IDENTIFIER [ '(' INPUT, OUTPUT ')' ] ';'
//  */
// bool ps_parse_program(ps_interpreter *interpreter, ps_interpreter_mode mode)
// {
//     VISIT_BEGIN("PROGRAM", "");
//     ps_identifier identifier;

//     EXPECT_TOKEN(PS_TOKEN_PROGRAM);
//     READ_NEXT_TOKEN;
//     EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
//     COPY_IDENTIFIER(identifier);
//     READ_NEXT_TOKEN;
//     // TODO Skip optional parameters enclosed in parentheses
//     EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
//     READ_NEXT_TOKEN;

//     if (!ps_interpreter_enter_environment(interpreter, &identifier))
//         RETURN_ERROR(interpreter->error);
//     ps_symbol *program = ps_symbol_alloc(PS_SYMBOL_KIND_PROGRAM, &identifier, NULL);
//     if (!ps_interpreter_add_symbol(interpreter, program))
//         RETURN_ERROR(interpreter->error);
//     if (!ps_parse_block(interpreter, mode))
//         TRACE_ERROR("BLOCK");
//     // ps_symbol_table_dump(ps_interpreter_get_environment(interpreter)->symbols, "Before EXIT", stderr);
//     ps_interpreter_exit_environment(interpreter);
//     EXPECT_TOKEN(PS_TOKEN_DOT);
//     // NB: text after '.' is not analyzed and has not to be

//     VISIT_END("OK");
// }

/**
 * Parse
 *  PROGRAM IDENTIFIER ';'
 *  BLOCK
 *  '.'
 */
bool ps_parse_start(ps_interpreter *interpreter, ps_interpreter_mode mode)
{
    // VISIT_BEGIN("START", "");

    // READ_NEXT_TOKEN;
    // if (!ps_parse_program(interpreter, mode))
    //     TRACE_ERROR("PROGRAM");

    // VISIT_END("OK");
    return false;
}
