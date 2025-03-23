#include <string.h>

#include "ps_parser.h"

#define GET_LEXER ps_lexer *lexer = ps_parser_get_lexer(parser)
#define READ_NEXT_TOKEN                   \
    if (!ps_lexer_read_next_token(lexer)) \
    return false
#define EXPECT_TOKEN(__TOKEN_TYPE__)                          \
    if (!ps_parser_expect_token_type(parser, __TOKEN_TYPE__)) \
    return false

/**
 * Visit PROGRAM IDENTIFIER;
 */
bool ps_parser_visit_program(ps_parser *parser)
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
        &lexer->current_token.value.identifier,
        NULL);
    ps_symbol_table_add(parser->symbols, program);
    return true;
}

/**
 * Visit CONST IDENTIFIER = VALUE;
 */
bool ps_parser_visit_block_const(ps_parser *parser)
{
    GET_LEXER;
    ps_identifier identifier;
    ps_type_definition *type;
    ps_value *value;
    ps_value_data data;
    ps_symbol *constant;
    EXPECT_TOKEN(TOKEN_CONST);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(TOKEN_IDENTIFIER);
    strncpy(identifier, lexer->current_token.value.identifier, PS_IDENTIFIER_LEN);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(TOKEN_EQUAL);
    READ_NEXT_TOKEN;
    // static const ps_token_type const_value_token_types[] = {TOKEN_INTEGER_VALUE, TOKEN_UNSIGNED_VALUE};
    // if (!ps_parser_expect_token_types(parser, sizeof(const_value_token_types) / sizeof(ps_token_type), const_value_token_types))
    //     return false;
    switch (lexer->current_token.type)
    {
    case TOKEN_INTEGER_VALUE:
        type = ps_symbol_integer.value->data.t;
        data.i = lexer->current_token.value.i;
        break;
    case TOKEN_UNSIGNED_VALUE:
        type = ps_symbol_unsigned.value->data.t;
        data.u = lexer->current_token.value.u;
        break;
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
    ps_symbol_table_add(parser->symbols, constant);
    // TODO loop if we have another identifier

    return true;
}

/**
 * Visit VAR IDENTIFIER : TYPE;
 */
bool ps_parser_visit_block_var(ps_parser *parser)
{
    GET_LEXER;
    ps_identifier identifier;
    ps_type_definition *type;
    ps_value *value;
    ps_value_data data;
    ps_symbol *var;
    EXPECT_TOKEN(TOKEN_VAR);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(TOKEN_IDENTIFIER);
    strncpy(identifier, lexer->current_token.value.identifier, PS_IDENTIFIER_LEN);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(TOKEN_COLON);
    READ_NEXT_TOKEN;
    switch (lexer->current_token.type)
    {
    case TOKEN_INTEGER:
        type = ps_symbol_integer.value->data.t;
        data.i = 0;
        break;
    case TOKEN_UNSIGNED:
        type = ps_symbol_unsigned.value->data.t;
        data.u = 0;
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
    if (ps_symbol_table_add(parser->symbols, var) == NULL)
        return false;
    // TODO loop if we have another identifier

    return true;
}

bool ps_parser_visit_main_block(ps_parser *parser)
{
    GET_LEXER;
    EXPECT_TOKEN(TOKEN_BEGIN);
    READ_NEXT_TOKEN;
    // TODO instruction block
    EXPECT_TOKEN(TOKEN_END);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(TOKEN_DOT);
    // NB: source code after '.' is not analyzed and has not to be
    return true;
}

bool ps_parser_start(ps_parser *parser)
{
    GET_LEXER;
    READ_NEXT_TOKEN;
    // Mandatory
    if (!ps_parser_visit_program(parser))
        return false;
    // Optional
    if (lexer->current_token.type == TOKEN_CONST && !ps_parser_visit_block_const(parser))
        return false;
    // Optional
    if (lexer->current_token.type == TOKEN_VAR && !ps_parser_visit_block_var(parser))
        return false;
    // Mandatory
    return ps_parser_visit_main_block(parser);
}
