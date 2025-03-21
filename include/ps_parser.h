/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_PARSER_H
#define _PS_PARSER_H

// #include "ps_config.h"
#include "ps_error.h"
#include "ps_token.h"
#include "ps_lexer.h"
// #include "ps_symbol.h"
#include "ps_symbol_table.h"

#ifdef __cplusplus
extern "C"
{
#endif

#define PS_PARSER_LEXER_COUNT 2

    typedef struct ps_parser
    {
        ps_lexer *lexers[PS_PARSER_LEXER_COUNT];
        uint8_t lexer;
        ps_error error;
        bool allocated_parser;
        bool allocated_symbol_table;
        ps_symbol_table *symbols;
    } ps_parser;

    /** @brief Initialize parser with attached symbol table */
    ps_parser *ps_parser_init(ps_parser *parser, ps_symbol_table *symbols);
    /** @brief Free parser & symbol table */
    void ps_parser_done(ps_parser *parser);
    ps_lexer *ps_parser_get_lexer(ps_parser *parser);
    bool ps_parser_expect_token_type(ps_parser *parser, ps_token_type token_type);
    ps_token_type ps_parser_expect_token_types(ps_parser *parser, size_t token_type_count, ps_token_type token_types[]);
    bool ps_parser_start(ps_parser *parser);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSER_H */
