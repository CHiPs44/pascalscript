/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_PARSER_H
#define _PS_PARSER_H

#include <stdbool.h>

#include "ps_config.h"
#include "ps_error.h"
#include "ps_token.h"
#include "ps_lexer.h"
#include "ps_symbol_table.h"

#ifdef __cplusplus
extern "C"
{
#endif

#define PS_PARSER_LEXER_COUNT 1

    typedef struct ps_parser
    {
        ps_lexer lexer[PS_PARSER_LEXER_COUNT];
        ps_error error;
        ps_symbol_table *symbol_table;
    } ps_parser;

    bool ps_parser_init(ps_parser *parser);
    bool ps_parser_expect_token_type(ps_parser *parser, ps_token_type token_type);
    bool ps_parser_expect_token_types(ps_parser *parser, size_t token_type_count, ps_token_type token_type[]);
    bool ps_parser_start(ps_parser *parser);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSER_H */
