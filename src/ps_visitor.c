#include <string.h>

#include "ps_system.h"
#include "ps_parser.h"

// clang-format off
#define GET_LEXER                       ps_lexer *lexer = ps_parser_get_lexer(parser)
#define READ_NEXT_TOKEN                 if (!ps_lexer_read_next_token(lexer)) return false; else if (parser->trace) ps_token_dump(&lexer->current_token)
#define EXPECT_TOKEN(__TOKEN_TYPE__)    if (!ps_parser_expect_token_type(parser, __TOKEN_TYPE__)) return false
#define RETURN_ERROR(__PS_ERROR__)      { parser->error = __PS_ERROR__; return false; }
#define COPY_IDENTIFIER(__IDENTIFIER__) strncpy(__IDENTIFIER__, lexer->current_token.value.identifier, PS_IDENTIFIER_LEN)
// clang-format on

/* cf. https://en.wikipedia.org/wiki/Comparison_of_Pascal_and_C#Expressions
    Level   Syntax Element      Operator
    ------- ------------------- -----------------
    0       factor              literal, variable
    1       signed factor       unary minus, NOT
    2       term                *, /, AND
    3       expression          +, -, OR
*/

bool ps_visit_factor(ps_parser *parser)
{
    return true;
}

bool ps_visit_factor(ps_parser *parser)
{
    return true;
}

bool ps_visit_factor(ps_parser *parser)
{
    return true;
}

bool ps_visit_expression(ps_parser *parser)
{
    
    return true;
}

/**
 * Visit PROGRAM IDENTIFIER;
 *
 * Next step : ignore?
 *       ( IDENTIFIER , IDENTIFIER ...)
 */
bool ps_visit_program(ps_parser *parser)
{
    ps_identifier identifier;
    GET_LEXER;
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
    if (ps_symbol_table_add(parser->symbols, program) == NULL)
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
bool ps_visit_block_const(ps_parser *parser)
{
    GET_LEXER;
    ps_identifier identifier;
    ps_type_definition *type;
    ps_value *value;
    ps_value_data data;
    ps_symbol *constant;
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
            constant = ps_symbol_table_find(parser->symbols, lexer->current_token.value.identifier);
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
        if (ps_symbol_table_add(parser->symbols, constant) == NULL)
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
bool ps_visit_block_var(ps_parser *parser)
{
    GET_LEXER;
    ps_identifier identifier;
    ps_type_definition *type;
    ps_value *value;
    ps_value_data data;
    ps_symbol *variable;
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
        if (ps_symbol_table_add(parser->symbols, variable) == NULL)
            RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_ADDED);
    } while (lexer->current_token.type == TOKEN_IDENTIFIER);
    return true;
}

/**
 * Visit instruction sequence
 *      WRITE | WRITELN ( IDENTIFIER ) ;
 *      ...
 * Next steps:
 *      IDENTIFIER := EXPRESSION ;
 *      WRITE | WRITELN ( EXPRESSION ) ;
 *      WRITE | WRITELN ( EXPRESSION , EXPRESSION ... ) ;
 */
bool ps_visit_instructions(ps_parser *parser)
{
    GET_LEXER;
    ps_identifier identifier;
    ps_symbol *symbol;
    char *display_value;

    switch (lexer->current_token.type)
    {
    case TOKEN_IDENTIFIER:
        COPY_IDENTIFIER(identifier);
        EXPECT_TOKEN(TOKEN_ASSIGN);
        // TODO
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED);
        break;
    case TOKEN_WRITE:
    case TOKEN_WRITELN:
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(TOKEN_LEFT_PARENTHESIS);
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(TOKEN_IDENTIFIER);
        COPY_IDENTIFIER(identifier);
        EXPECT_TOKEN(TOKEN_RIGHT_PARENTHESIS);
        READ_NEXT_TOKEN;
        // start "code" execution⁼
        symbol = ps_symbol_table_get(parser->symbols, identifier);
        if (symbol == NULL)
            RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_FOUND);
        display_value = ps_value_get_display_value(symbol->value);
        if (display_value == NULL)
            RETURN_ERROR(PS_RUNTIME_ERROR_EXPECTED_STRING);
        if (lexer->current_token.type == TOKEN_WRITELN)
            printf("%s\n", display_value);
        // end "code" execution
        break;
    default:
        RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN);
    }

    return true;
}

/**
 * Visit [ CONST ... | TYPE ... ]
 *       BEGIN
 *         [ INSTRUCTION ... ]
 *       END
 * NB: ; or . after END is analyzed in the caller
 */
bool ps_visit_block(ps_parser *parser)
{
    GET_LEXER;
    do
    {
        if (lexer->current_token.type == TOKEN_CONST && !ps_visit_block_const(parser))
            return false;
        // if (lexer->current_token.type == TOKEN_TYPE && !ps_visit_block_type(parser))
        //     return false;
        if (lexer->current_token.type == TOKEN_VAR && !ps_visit_block_var(parser))
            return false;
    } while (lexer->current_token.type != TOKEN_BEGIN);
    EXPECT_TOKEN(TOKEN_BEGIN);
    READ_NEXT_TOKEN;
    if (lexer->current_token.type != TOKEN_END && !ps_visit_instructions(parser))
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
bool ps_parser_start(ps_parser *parser)
{
    GET_LEXER;
    READ_NEXT_TOKEN;
    if (!ps_visit_program(parser))
        return false;
    if (!ps_visit_block(parser))
        return false;
    EXPECT_TOKEN(TOKEN_DOT);
    // NB: source code after '.' is not analyzed and has not to be
    return true;
}
