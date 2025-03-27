/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "ps_parser.h"

ps_parser *ps_parser_init(ps_parser *parser, ps_symbol_table *symbols)
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
        parser->allocated_parser = false;
    }
    for (uint8_t i = 0; i < PS_PARSER_LEXER_COUNT; i++)
    {
        parser->lexers[i] = ps_lexer_init(NULL);
    }
    parser->current_lexer = 0;
    if (symbols == NULL)
    {
        parser->allocated_symbol_table = true;
        parser->symbols = ps_symbol_table_init(NULL);
    }
    else
    {
        parser->allocated_symbol_table = false;
        parser->symbols = symbols;
    }
    parser->error = PS_PARSER_ERROR_NONE;
    parser->trace = false;
    parser->debug = false;
    return parser;
}

void ps_parser_done(ps_parser *parser)
{
    for (size_t i = 0; i < PS_PARSER_LEXER_COUNT; i++)
        if (parser->lexers[i] != NULL)
        {
            ps_lexer_done(parser->lexers[i]);
            parser->lexers[i] = NULL;
        }
    if (parser->allocated_symbol_table)
    {
        ps_symbol_table_done(parser->symbols);
        parser->symbols = NULL;
    }
    if (parser->allocated_parser)
        free(parser);
}

bool ps_parser_use_lexer(ps_parser *parser, uint8_t current_lexer)
{
    if (current_lexer > PS_PARSER_LEXER_COUNT || parser->lexers[current_lexer] == NULL)
        return false;
    parser->current_lexer = current_lexer;
    return true;
}

ps_lexer *ps_parser_get_lexer(ps_parser *parser)
{
    return parser->lexers[parser->current_lexer];
}

void ps_parser_debug(ps_parser *parser, char *message)
{
    if (message == NULL)
        fprintf(stderr, "ERROR %s\n", ps_error_get_message(parser->error));
    else
        fprintf(stderr, "%s %s\n", message, ps_error_get_message(parser->error));
}

bool ps_parser_expect_token_type(ps_parser *parser, ps_token_type token_type)
{
    if (ps_parser_get_lexer(parser)->current_token.type != token_type)
    {
        parser->error = PS_PARSER_ERROR_UNEXPECTED_TOKEN;
        if (parser->debug)
            ps_parser_debug(parser, NULL);
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
