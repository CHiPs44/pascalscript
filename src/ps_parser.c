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
    parser->lexer[0] = ps_lexer_init(NULL);
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
    ps_lexer_done(parser->lexer);
    if (parser->allocated_symbol_table)
        ps_symbol_table_done(parser->symbol_table);
    if (!parser->allocated_parser)
        free(parser);
}

bool ps_parser_expect_token_type(ps_parser *parser, ps_token_type token_type)
{
    if (parser->lexer[0]->current_token.type != token_type)
    {
        parser->error = PS_PARSER_ERROR_UNEXPECTED_TOKEN;
        return false;
    }
    return true;
}

bool ps_parser_start(ps_parser *parser)
{
    ps_symbol *program = ps_symbol_init(
        PS_SYMBOL_SCOPE_GLOBAL,
        PS_SYMBOL_KIND_PROGRAM, NULL, NULL);

    if (!ps_parser_expect_token_type(parser, TOKEN_PROGRAM))
    {
        return false;
    }
    if (!ps_parser_expect_token_type(parser, TOKEN_IDENTIFIER))
    {
        return false;
    }
    // program->value->type = &ps_type_def_integer;
    // program->value->data.s = calloc(1, sizeof(ps_string));
    // size_t len = strlen(parser->lexer[0]->current_token.value.s);
    // program.value.data.s->max = len;
    // program.value.data.s->len = len;
    // program.value.data.s.str = calloc(len + 1, sizeof(ps_char));
    // strcpy(program.value.data.s->str, parser->lexer[0]->current_token.value.s);
    // ps_symbol_table_add(&vm->symbols, &program);

    return true;
}
