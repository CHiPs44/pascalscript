/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_PARSER_H
#define _PS_PARSER_H

#include "ps_config.h"
#include "ps_error.h"
#include "ps_lexer.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_token.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef PS_PARSER_LEXER_COUNT
#define PS_PARSER_LEXER_COUNT 8
#endif

    typedef struct ps_parser
    {
        ps_lexer *lexers[PS_PARSER_LEXER_COUNT];
        int current_lexer;
        ps_error error;
        bool trace;
        bool debug;
    } ps_parser;

#define PS_PARSER_SIZE sizeof(ps_parser)

    /** @brief Initialize parse */
    ps_parser *ps_parser_alloc(void);
    /** @brief Free parser & lexers */
    ps_parser *ps_parser_free(ps_parser *parser);
    /** @brief Get current lexer */
    ps_lexer *ps_parser_get_lexer(const ps_parser *parser);
    /** @brief Expect token type */
    bool ps_parser_expect_token_type(ps_parser *parser, ps_token_type token_type);
    /** @brief Expect token types */
    ps_token_type ps_parser_expect_token_types(const ps_parser *parser, size_t token_type_count, const ps_token_type token_types[]);
    /** @brief Expect end of statement token: ';', END, ELSE, UNTIL */
    ps_token_type ps_parser_expect_statement_end_token(const ps_parser *parser);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSER_H */
