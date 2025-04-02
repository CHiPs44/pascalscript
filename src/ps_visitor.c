#include <string.h>

#include "ps_functions.h"
#include "ps_system.h"
#include "ps_parser.h"
// #include "ps_visitor.h"

// clang-format off
#define GET_LEXER                       ps_lexer *lexer = ps_parser_get_lexer(interpreter->parser)
#define READ_NEXT_TOKEN                 if (!ps_lexer_read_next_token(lexer)) return false; else \
                                        if (interpreter->parser->trace) ps_token_dump(&lexer->current_token)
#define EXPECT_TOKEN(__TOKEN_TYPE__)    if (!ps_parser_expect_token_type(interpreter->parser, __TOKEN_TYPE__)) return false
#define RETURN_ERROR(__PS_ERROR__)      { interpreter->error = __PS_ERROR__; return false; }
#define COPY_IDENTIFIER(__IDENTIFIER__) strncpy(__IDENTIFIER__, lexer->current_token.value.identifier, PS_IDENTIFIER_LEN)
// clang-format on

// bool ps_visit_(ps_interpreter *interpreter)
// {
//     return true;
// }

bool ps_visit_factor(ps_interpreter *interpreter, ps_value *result)
{
    GET_LEXER;
    ps_value factor;
    ps_identifier identifier;
    ps_symbol *symbol;
    if (interpreter->parser->debug)
        fprintf(stderr, "*** FACTOR\n");
    switch (lexer->current_token.type)
    {
    case TOKEN_IDENTIFIER:
        // TODO: variable, const, function, ...
        // interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        // return false;
        COPY_IDENTIFIER(identifier);
        READ_NEXT_TOKEN;
        symbol = ps_symbol_table_get(interpreter->parser->symbols, &identifier);
        if (symbol == NULL)
            RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_FOUND);
        result->type = symbol->value->type;
        result->data = symbol->value->data;
        return true;
    case TOKEN_CHAR_VALUE:
        result->type = ps_symbol_char.value->data.t;
        result->data.c = lexer->current_token.value.c;
        READ_NEXT_TOKEN;
        return true;
    case TOKEN_INTEGER_VALUE:
        result->type = ps_symbol_integer.value->data.t;
        result->data.i = lexer->current_token.value.i;
        READ_NEXT_TOKEN;
        return true;
    case TOKEN_UNSIGNED_VALUE:
        result->type = ps_symbol_unsigned.value->data.t;
        result->data.u = lexer->current_token.value.u;
        READ_NEXT_TOKEN;
        return true;
    case TOKEN_REAL_VALUE:
        result->type = ps_symbol_real.value->data.t;
        result->data.r = lexer->current_token.value.r;
        READ_NEXT_TOKEN;
        return true;
    case TOKEN_BOOLEAN_VALUE:
        result->type = ps_symbol_boolean.value->data.t;
        result->data.b = lexer->current_token.value.b;
        READ_NEXT_TOKEN;
        return true;
    case TOKEN_MINUS:
        READ_NEXT_TOKEN;
        if (!ps_visit_factor(interpreter, &factor))
            return false;
        if (!ps_function_neg(interpreter, &factor, result))
            return false;
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        READ_NEXT_TOKEN;
        return true;
    case TOKEN_STRING_VALUE:
    case TOKEN_NIL:
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        return false;
    default:
        interpreter->error = PS_PARSER_ERROR_UNEXPECTED_TOKEN;
        return false;
    }
}

bool ps_visit_term(ps_interpreter *interpreter, ps_value *result)
{
    if (interpreter->parser->debug)
        fprintf(stderr, "*** TERM\n");
    if (!ps_visit_factor(interpreter, result))
        return false;
    return true;
    // interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
    // return false;
}

bool ps_visit_simple_expression(ps_interpreter *interpreter, ps_value *result)
{
    if (interpreter->parser->debug)
        fprintf(stderr, "*** SIMPLE_EXPRESSION\n");
    if (!ps_visit_term(interpreter, result))
        return false;
    return true;
    // interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
    // return false;
}

// static ps_token_type relational_operators[] = {
//     TOKEN_LESS_THAN,
//     TOKEN_LESS_OR_EQUAL,
//     TOKEN_GREATER_THAN,
//     TOKEN_GREATER_OR_EQUAL,
//     TOKEN_EQUAL,
//     TOKEN_NOT_EQUAL,
//     TOKEN_IN,
//     // TOKEN_IS,
// };

bool ps_visit_expression(ps_interpreter *interpreter, ps_value *result)
{
    GET_LEXER;
    ps_value left = {0}; //, right = {0};
    // ps_token_type relational_operator;
    if (!ps_visit_simple_expression(interpreter, &left))
        return false;
    if (lexer->current_token.type == TOKEN_SEMI_COLON ||
        lexer->current_token.type == TOKEN_ELSE ||
        lexer->current_token.type == TOKEN_RIGHT_PARENTHESIS)
    {
        result->type = left.type;
        result->data = left.data;
        return true;
    }
    // No right part for now
    interpreter->parser->error = PS_ERROR_NOT_IMPLEMENTED;
    return false;
    // relational_operator = ps_parser_expect_token_types(
    //     interpreter->parser,
    //     sizeof(relational_operators) / sizeof(ps_token_type),
    //     relational_operators);
    // if (relational_operator == TOKEN_NONE)
    // {
    //     interpreter->parser = PS_PARSER_ERROR_UNEXPECTED_TOKEN;
    //     return false;
    // }
    // if (!ps_visit_simple_expression(interpreter, &right))
    //     return false;
    // // ...
    // return true;
}

/**
 * Visit PROGRAM IDENTIFIER;
 *
 * Next step : ignore?
 *       ( IDENTIFIER , IDENTIFIER ...)
 */
bool ps_visit_program(ps_interpreter *interpreter)
{
    ps_identifier identifier;
    GET_LEXER;
    if (interpreter->parser->debug)
        fprintf(stderr, "*** PROGRAM\n");
    EXPECT_TOKEN(TOKEN_PROGRAM);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(TOKEN_IDENTIFIER);
    COPY_IDENTIFIER(identifier);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(TOKEN_SEMI_COLON);
    READ_NEXT_TOKEN;
    ps_symbol *program = ps_symbol_init(
        PS_SYMBOL_SCOPE_GLOBAL,
        PS_SYMBOL_KIND_PROGRAM,
        &identifier,
        NULL);
    if (ps_symbol_table_add(interpreter->parser->symbols, program) == NULL)
        RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_ADDED);
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
bool ps_visit_block_const(ps_interpreter *interpreter)
{
    GET_LEXER;
    ps_identifier identifier;
    ps_type_definition *type;
    ps_value *value;
    ps_value_data data;
    ps_symbol *constant;
    if (interpreter->parser->debug)
        fprintf(stderr, "*** CONST\n");
    EXPECT_TOKEN(TOKEN_CONST);
    READ_NEXT_TOKEN;
    do
    {
        EXPECT_TOKEN(TOKEN_IDENTIFIER);
        COPY_IDENTIFIER(identifier);
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(TOKEN_EQUAL);
        READ_NEXT_TOKEN;
        switch (lexer->current_token.type)
        {
        case TOKEN_IDENTIFIER:
            constant = ps_symbol_table_get(interpreter->parser->symbols, &lexer->current_token.value.identifier);
            if (constant == NULL)
                RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_FOUND);
            if (constant->kind != PS_SYMBOL_KIND_CONSTANT)
                RETURN_ERROR(PS_RUNTIME_ERROR_EXPECTED_CONSTANT);
            type = constant->value->type;
            data = constant->value->data;
            break;
        case TOKEN_INTEGER_VALUE:
            type = ps_symbol_integer.value->data.t;
            data.i = lexer->current_token.value.i;
            break;
        case TOKEN_REAL_VALUE:
            type = ps_symbol_real.value->data.t;
            data.r = lexer->current_token.value.r;
            break;
        case TOKEN_UNSIGNED_VALUE:
            type = ps_symbol_unsigned.value->data.t;
            data.u = lexer->current_token.value.u;
            break;
        case TOKEN_CHAR_VALUE:
            type = ps_symbol_char.value->data.t;
            data.c = lexer->current_token.value.c;
            break;
        case TOKEN_BOOLEAN_VALUE:
            type = ps_symbol_boolean.value->data.t;
            data.b = lexer->current_token.value.b;
            break;
        // Not yet!
        // case TOKEN_STRING_VALUE:
        //     type = ps_symbol_string.value->data.t;
        //     strncpy(data.s + 1, lexer->current_token.value.s, PS_STRING_MAX_LEN);
        //     break;
        default:
            RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN);
        }
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN;
        value = ps_value_init(type, data);
        constant = ps_symbol_init(
            PS_SYMBOL_SCOPE_GLOBAL,
            PS_SYMBOL_KIND_CONSTANT,
            &identifier,
            value);
        if (constant == NULL)
        {
            RETURN_ERROR(PS_RUNTIME_ERROR_OUT_OF_MEMORY);
        }
        if (ps_symbol_table_add(interpreter->parser->symbols, constant) == NULL)
            RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_ADDED);
    } while (lexer->current_token.type == TOKEN_IDENTIFIER);
    return true;
}

/**
 * Visit    VAR IDENTIFIER : TYPE;
 *              IDENTIFIER : TYPE;
 *          ...
 * Next step: allow identifier list with commas
 *              IDENTIFIER, IDENTIFIER, ... : TYPE ;
 */
bool ps_visit_block_var(ps_interpreter *interpreter)
{
    GET_LEXER;
    ps_identifier identifier;
    ps_type_definition *type;
    ps_value *value;
    ps_value_data data;
    ps_symbol *variable;
    if (interpreter->parser->debug)
        fprintf(stderr, "*** VAR\n");
    EXPECT_TOKEN(TOKEN_VAR);
    READ_NEXT_TOKEN;
    do
    {
        EXPECT_TOKEN(TOKEN_IDENTIFIER);
        COPY_IDENTIFIER(identifier);
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(TOKEN_COLON);
        READ_NEXT_TOKEN;
        switch (lexer->current_token.type)
        {
        case TOKEN_BOOLEAN:
            type = ps_symbol_boolean.value->data.t;
            data.b = (ps_boolean) false;
            break;
        case TOKEN_CHAR:
            type = ps_symbol_char.value->data.t;
            data.c = '\0';
            break;
        case TOKEN_INTEGER:
            type = ps_symbol_integer.value->data.t;
            data.i = 0;
            break;
        case TOKEN_UNSIGNED:
            type = ps_symbol_unsigned.value->data.t;
            data.u = 0;
            break;
        case TOKEN_REAL:
            type = ps_symbol_real.value->data.t;
            data.r = 0.0;
            break;
        default:
            RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN);
        }
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN;
        value = ps_value_init(type, data);
        variable = ps_symbol_init(
            PS_SYMBOL_SCOPE_GLOBAL,
            PS_SYMBOL_KIND_VARIABLE,
            &identifier,
            value);
        if (variable == NULL)
            RETURN_ERROR(PS_RUNTIME_ERROR_OUT_OF_MEMORY);
        if (ps_symbol_table_add(interpreter->parser->symbols, variable) == NULL)
            RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_ADDED);
    } while (lexer->current_token.type == TOKEN_IDENTIFIER);
    return true;
}

/**
 * Visit statement sequence
 *      IDENTIFIER := EXPRESSION ;
 *      WRITE | WRITELN ( EXPRESSION ) ;
 * Next steps:
 *      WRITE | WRITELN ( EXPRESSION , EXPRESSION ... ) ;
 *      IF EXPRESSION THEN STATEMENT ;
 *      IF EXPRESSION THEN STATEMENT ELSE STATEMENT ;
 *      IF EXPRESSION THEN BEGIN STATEMENTS END ELSE STATEMENT ;
 *      IF EXPRESSION THEN BEGIN STATEMENTS END ELSE BEGIN STATEMENTS END ;
 *      IF EXPRESSION THEN STATEMENT ELSE BEGIN STATEMENTS END ;
 */
bool ps_visit_statements(ps_interpreter *interpreter)
{
    GET_LEXER;
    ps_identifier identifier;
    ps_symbol *symbol;
    ps_value result;
    char *display_value;
    if (interpreter->parser->debug)
        fprintf(stderr, "*** STATEMENTS\n");
    do
    {
        switch (lexer->current_token.type)
        {
        case TOKEN_IDENTIFIER:
            /* IDENTIFIER := EXPRESSION ; */
            COPY_IDENTIFIER(identifier);
            READ_NEXT_TOKEN;
            EXPECT_TOKEN(TOKEN_ASSIGN);
            READ_NEXT_TOKEN;
            if (!ps_visit_expression(interpreter, &result))
                return false;
            // start "code" execution
            symbol = ps_symbol_table_get(interpreter->parser->symbols, &identifier);
            if (symbol == NULL)
            {
                interpreter->parser->error = PS_RUNTIME_ERROR_SYMBOL_NOT_FOUND;
                return false;
            }
            ps_value_debug(stderr, "ASSIGN => ", &result);
            if (result.type != symbol->value->type)
            {
                interpreter->parser->error = PS_RUNTIME_ERROR_TYPE_MISMATCH;
                return false;
            }
            symbol->value->data = result.data;
            // end "code" execution
            EXPECT_TOKEN(TOKEN_SEMI_COLON);
            READ_NEXT_TOKEN;
            break;
        case TOKEN_WRITE:
        case TOKEN_WRITELN:
            /* WRITE[LN] ( EXPRESSION ) ; */
            bool newline = lexer->current_token.type == TOKEN_WRITELN;
            READ_NEXT_TOKEN;
            EXPECT_TOKEN(TOKEN_LEFT_PARENTHESIS);
            READ_NEXT_TOKEN;
            if (!ps_visit_expression(interpreter, &result))
                return false;
            EXPECT_TOKEN(TOKEN_RIGHT_PARENTHESIS);
            READ_NEXT_TOKEN;
            // start "code" execution⁼
            display_value = ps_value_get_display_value(&result);
            if (display_value == NULL)
                RETURN_ERROR(PS_RUNTIME_ERROR_EXPECTED_STRING);
            if (newline)
                printf("%s\n", display_value);
            else
                printf("%s", display_value);
            // end "code" execution
            EXPECT_TOKEN(TOKEN_SEMI_COLON);
            READ_NEXT_TOKEN;
            break;
        case TOKEN_END:
            READ_NEXT_TOKEN;
            return true;
        default:
            RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN);
        }
    } while (true);
    return true;
}

/**
 * Visit [ CONST ... | TYPE ... ]
 *       BEGIN
 *         [ STATEMENT ... ]
 *       END
 * NB: ; or . after END is analyzed in the caller
 */
bool ps_visit_block(ps_interpreter *interpreter)
{
    GET_LEXER;
    if (interpreter->parser->debug)
        fprintf(stderr, "*** BLOCK\n");
    do
    {
        if (lexer->current_token.type == TOKEN_CONST && !ps_visit_block_const(interpreter))
            return false;
        // if (lexer->current_token.type == TOKEN_TYPE && !ps_visit_block_type(interpreter))
        //     return false;
        if (lexer->current_token.type == TOKEN_VAR && !ps_visit_block_var(interpreter))
            return false;
    } while (lexer->current_token.type != TOKEN_BEGIN);
    EXPECT_TOKEN(TOKEN_BEGIN);
    READ_NEXT_TOKEN;
    if (lexer->current_token.type != TOKEN_END && !ps_visit_statements(interpreter))
        return false;
    EXPECT_TOKEN(TOKEN_END);
    READ_NEXT_TOKEN;
    return true;
}

/**
 * Visit PROGRAM...
 *       BLOCK
 *       .
 */
bool ps_visit_start(ps_interpreter *interpreter)
{
    GET_LEXER;
    if (interpreter->parser->debug)
        fprintf(stderr, "*** START\n");
    READ_NEXT_TOKEN;
    if (!ps_visit_program(interpreter))
        return false;
    if (!ps_visit_block(interpreter))
        return false;
    EXPECT_TOKEN(TOKEN_DOT);
    // NB: source code after '.' is not analyzed and has not to be
    return true;
}
