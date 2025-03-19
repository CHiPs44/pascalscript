/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdlib.h>
#include <string.h>

#include "ps_lexer.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_parser.h"

ps_parser *ps_parser_init(ps_parser *parser, ps_symbol_table *symbol_table)
{
    if (parser == NULL)
    {
        parser = calloc(1, sizeof(ps_parser));
        if (parser == NULL)
            return NULL;
        parser->allocated_parser = true;
    }
    else
    {
        parser->allocated_parser = true;
    }
    for (uint8_t i = 0; i < PS_PARSER_LEXER_COUNT; i++)
    {
        parser->lexers[i] = ps_lexer_init(NULL);
    }
    parser->lexer = 0;
    if (symbol_table == NULL)
    {
        parser->allocated_symbol_table = true;
        parser->symbol_table = ps_symbol_table_init(NULL);
    }
    else
    {
        parser->allocated_symbol_table = false;
        parser->symbol_table = symbol_table;
    }
    parser->error = PS_PARSER_ERROR_NONE;
    return true;
}

void ps_parser_done(ps_parser *parser)
{
    ps_lexer_done(parser->lexers);
    if (parser->allocated_symbol_table)
        ps_symbol_table_done(parser->symbol_table);
    if (!parser->allocated_parser)
        free(parser);
}

bool ps_parser_use_lexer(ps_parser *parser, uint8_t lexer)
{
    if (lexer > PS_PARSER_LEXER_COUNT)
        return false;
    parser->lexer = lexer;
    return true;
}

ps_lexer *ps_parser_get_lexer(ps_parser *parser)
{
    return parser->lexers[parser->lexer];
}

bool ps_parser_expect_token_type(ps_parser *parser, ps_token_type token_type)
{
    if (ps_parser_get_lexer(parser)->current_token.type != token_type)
    {
        parser->error = PS_PARSER_ERROR_UNEXPECTED_TOKEN;
        return false;
    }
    return true;
}

ps_token_type ps_parser_expect_token_types(ps_parser *parser, size_t token_type_count, ps_token_type token_types[])
{
    ps_token_type token_type = ps_parser_get_lexer(parser)->current_token.type;
    for (size_t i = 0; i < token_type_count; i++)
    {
        if (token_type == token_types[i])
            return token_type;
    }
    return TOKEN_NONE;
}

bool ps_parser_start(ps_parser *parser)
{
    ps_lexer *lexer = ps_parser_get_lexer(parser);
    if (!ps_lexer_read_next_token(lexer))
        return false;
    return ps_parser_parse_program(parser);
}

/**
 * Parse PROGRAM IDENTIFIER;
 */
bool ps_parser_parse_program(ps_parser *parser)
{
    ps_lexer *lexer = ps_parser_get_lexer(parser);
    if (!ps_parser_expect_token_type(parser, TOKEN_PROGRAM))
    {
        return false;
    }
    if (!ps_lexer_read_next_token(lexer))
        return false;
    if (!ps_parser_expect_token_type(parser, TOKEN_IDENTIFIER))
    {
        return false;
    }
    if (!ps_lexer_read_next_token(lexer))
        return false;
    if (!ps_parser_expect_token_type(parser, TOKEN_SEMI_COLON))
        return false;
    if (!ps_lexer_read_next_token(lexer))
        return false;
    ps_symbol *program = ps_symbol_init(
        PS_SYMBOL_SCOPE_GLOBAL,
        PS_SYMBOL_KIND_PROGRAM,
        ps_parser_get_lexer(parser)->current_token.value.identifier,
        NULL);
    ps_symbol_table_add(parser->symbol_table, &program);

    return true;
}

/**
 * Parse CONST IDENTIFIER = VALUE;
 *             ...
 *             IDENTIFIER = VALUE;
 */
ps_token_type const_value_token_types = {TOKEN_INTEGER_VALUE, TOKEN_CARDINAL_VALUE};

bool ps_parser_parse_const(ps_parser *parser)
{
    ps_lexer *lexer = ps_parser_get_lexer(parser);
    ps_identifier identifier;
    ps_value *value;
    ps_symbol *constant;
    if (!ps_parser_expect_token_type(parser, TOKEN_CONST))
    {
        return false;
    }
    if (!ps_lexer_read_next_token(lexer))
        return false;
    if (!ps_parser_expect_token_type(parser, TOKEN_IDENTIFIER))
        return false;
    strncpy(identifier, lexer->current_token.value.identifier, PS_IDENTIFIER_LEN);
    if (!ps_lexer_read_next_token(lexer))
        return false;
    if (!ps_parser_expect_token_type(parser, TOKEN_EQUAL))
        return false;
    if (!ps_lexer_read_next_token(lexer))
        return false;
    if (!ps_parser_expect_token_types(parser, 2, const_value_token_types))
        return false;
    if (!ps_lexer_read_next_token(lexer))
        return false;
    if (!ps_parser_expect_token_type(parser, TOKEN_SEMI_COLON))
        return false;
    if (!ps_lexer_read_next_token(lexer))
        return false;
    // TODO loop if identifier
    // ps_value=ps_value_
    constant = ps_symbol_init(
        PS_SYMBOL_SCOPE_GLOBAL,
        PS_SYMBOL_KIND_CONSTANT,
        identifier,
        value);
    ps_symbol_table_add(parser->symbol_table, &constant);



    return true;
}