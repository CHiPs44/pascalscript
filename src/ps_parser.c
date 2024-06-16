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

bool parser_init(parser_t *parser)
{
    ps_lexer_init(&parser->lexer);
    memset(&parser->current_token, 0, sizeof(token_t));
    parser->current_token.type = TOKEN_NONE;
    parser->error = PARSER_ERROR_NONE;
    return true;
}

bool parser_expect_token_type(parser_t *parser, token_type_t token_type)
{
    if (parser->current_token.type != token_type)
    {
        parser->error = PARSER_ERROR_UNEXPECTED;
        return false;
    }
    return true;
}

bool parser_start(parser_t *parser)
{
    symbol_t program;

    // if (!parser_expect_token_type(vm, TOKEN_PROGRAM))
    // {
    //     return false;
    // }
    // if (!parser_expect_token_type(vm, TOKEN_IDENTIFIER))
    // {
    //     return false;
    // }
    // program.kind = KIND_CONSTANT;
    // strcpy(program.name, "PROGRAM");
    // program.size = 0;
    // program.type = PS_TYPE_STRING;
    // strcpy(program.value.s, vm->current_token.value.s);
    // symbol_table_add(&vm->symbols, &program);

    return true;
}
