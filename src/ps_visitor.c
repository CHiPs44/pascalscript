#include <string.h>

#include "ps_system.h"
#include "ps_parser.h"

// clang-format off
#define GET_LEXER                    ps_lexer *lexer = ps_parser_get_lexer(parser)
#define READ_NEXT_TOKEN              if (!ps_lexer_read_next_token(lexer)) return false; else if (parser->trace) ps_token_dump(&lexer->current_token)
#define EXPECT_TOKEN(__TOKEN_TYPE__) if (!ps_parser_expect_token_type(parser, __TOKEN_TYPE__)) return false
// clang-format on

/**
 * Visit PROGRAM IDENTIFIER;
 */
bool ps_visit_program(ps_parser *parser)
{
    ps_identifier identifier;

    GET_LEXER;
    EXPECT_TOKEN(TOKEN_PROGRAM);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(TOKEN_IDENTIFIER);
    strncpy(identifier, lexer->current_token.value.identifier, PS_IDENTIFIER_LEN);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(TOKEN_SEMI_COLON);
    READ_NEXT_TOKEN;
    ps_symbol *program = ps_symbol_init(
        PS_SYMBOL_SCOPE_GLOBAL,
        PS_SYMBOL_KIND_PROGRAM,
        &identifier,
        NULL);
    ps_symbol_table_add(parser->symbols, program);
    return true;
}

/**
 * Visit CONST IDENTIFIER = VALUE;
 *             IDENTIFIER = VALUE;
 *             ...
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
        strncpy(identifier, lexer->current_token.value.identifier, PS_IDENTIFIER_LEN);
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(TOKEN_EQUAL);
        READ_NEXT_TOKEN;
        switch (lexer->current_token.type)
        {
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
            parser->error = PS_PARSER_ERROR_UNEXPECTED_TOKEN;
            return false;
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
            parser->error = PS_RUNTIME_ERROR_OUT_OF_MEMORY;
            return false;
        }
        if (ps_symbol_table_add(parser->symbols, constant) == NULL)
            return false;
    } while (lexer->current_token.type == TOKEN_IDENTIFIER);
    return true;
}

/**
 * Visit VAR IDENTIFIER : TYPE;
 *           IDENTIFIER : TYPE;
 *           ...
 */
bool ps_visit_block_var(ps_parser *parser)
{
    GET_LEXER;
    ps_identifier identifier;
    ps_type_definition *type;
    ps_value *value;
    ps_value_data data;
    ps_symbol *var;
    EXPECT_TOKEN(TOKEN_VAR);
    READ_NEXT_TOKEN;
    do
    {
        EXPECT_TOKEN(TOKEN_IDENTIFIER);
        strncpy(identifier, lexer->current_token.value.identifier, PS_IDENTIFIER_LEN);
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
            parser->error = PS_PARSER_ERROR_UNEXPECTED_TOKEN;
            return false;
        }
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN;
        value = ps_value_init(type, data);
        var = ps_symbol_init(
            PS_SYMBOL_SCOPE_GLOBAL,
            PS_SYMBOL_KIND_VARIABLE,
            &identifier,
            value);
        if (var == NULL)
        {
            parser->error = PS_RUNTIME_ERROR_OUT_OF_MEMORY;
            return false;
        }
        if (ps_symbol_table_add(parser->symbols, var) == NULL)
            return false;
    } while (lexer->current_token.type == TOKEN_IDENTIFIER);
    return true;
}

/**
 * Visit instruction sequence
 *      IDENTIFIER := EXPRESSION ;
 *      WRITELN ( IDENTIFIER ) ;
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
        strncpy(identifier, lexer->current_token.value.identifier, PS_IDENTIFIER_LEN);
        EXPECT_TOKEN(TOKEN_ASSIGN);
        // TODO
        parser->error = PS_ERROR_NOT_IMPLEMENTED;
        return false;
        break;
    case TOKEN_WRITELN:
        EXPECT_TOKEN(TOKEN_LEFT_PARENTHESIS);
        EXPECT_TOKEN(TOKEN_IDENTIFIER);
        strncpy(identifier, lexer->current_token.value.identifier, PS_IDENTIFIER_LEN);
        EXPECT_TOKEN(TOKEN_RIGHT_PARENTHESIS);
        symbol = ps_symbol_table_get(parser->symbols, identifier);
        if (symbol == NULL)
        {
            parser->error = PS_RUNTIME_ERROR_GLOBAL_TABLE_NOT_FOUND;
            return false;
        }
        display_value = ps_value_get_display_value(symbol->value);
        if (display_value == NULL)
        {
            parser->error = PS_RUNTIME_ERROR_EXPECTED_STRING;
            return false;
        }
        printf("%s\n", display_value);
        break;
    default:
        parser->error = PS_PARSER_ERROR_UNEXPECTED_TOKEN;
        return false;
    }

    return true;
}

/**
 * Visit [ CONST ... | TYPE ... ]
 *       BEGIN
 *         [ INSTRUCTION ... ]
 *       END
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

bool ps_parser_start(ps_parser *parser)
{
    GET_LEXER;
    READ_NEXT_TOKEN;
    if (!ps_visit_program(parser))
        return false;
    fprintf(stderr, "PROGRAM OK\n");
    if (!ps_visit_block(parser))
        return false;
    EXPECT_TOKEN(TOKEN_DOT);
    // NB: source code after '.' is not analyzed and has not to be
    return true;
}
