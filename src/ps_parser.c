/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdlib.h>
#include <string.h>

#include "ps_lexer.h"
#include "ps_symbol.h"
#include "ps_parser.h"

bool ps_parser_init(ps_parser *parser)
{
    ps_lexer_init(&parser->lexer);
    ps_symbol_table_init(&parser->symbol_table);
    parser->error = PS_PARSER_ERROR_NONE;
    return true;
}

bool ps_parser_expect_token_type(ps_parser *parser, ps_token_type token_type)
{
    if (parser->lexer.current_token.type != token_type)
    {
        parser->error = PS_PARSER_ERROR_UNEXPECTED_TOKEN;
        return false;
    }
    return true;
}

bool ps_parser_start(ps_parser *parser)
{
    ps_symbol program;

    if (!ps_parser_expect_token_type(parser, TOKEN_PROGRAM))
    {
        return false;
    }
    if (!ps_parser_expect_token_type(parser, TOKEN_IDENTIFIER))
    {
        return false;
    }
    program.type = PS_SYMBOL_TYPE_CONSTANT;
    strcpy(program.name, "PROGRAM");
    program.value.size = 0;
    program.value.type = PS_TYPE_STRING;
    program.value.data.s = calloc(1, sizeof(ps_string));
    size_t len = strlen(parser->lexer.current_token.value.s);
    program.value.data.s->max = len;
    program.value.data.s->len = len;
    program.value.data.s->str = calloc(len+1,sizeof(ps_char));
    strcpy(program.value.data.s->str, parser->lexer.current_token.value.s);
    ps_symbol_table_add(&vm->symbols, &program);

    return true;
}

void ps_parser_set_program(ps_parser *parser, char *name)
{

}
