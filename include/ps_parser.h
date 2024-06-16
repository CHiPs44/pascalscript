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

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct _parser_t
    {
        lexer_t lexer;
        token_t current_token;
        error_t error;
    } parser_t;

    bool parser_init(parser_t *parser);
    bool parser_expect_token_type(parser_t *parser, token_type_t token_type);
    bool parser_expect_token_types(parser_t *parser, size_t token_type_count, token_type_t token_type[]);
    bool parser_start(parser_t *parser);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARSER_H */
