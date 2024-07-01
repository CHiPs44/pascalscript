/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_LEXER_H
#define _PS_LEXER_H

#include <stdbool.h>

#include "ps_config.h"
#include "ps_error.h"
#include "ps_buffer.h"
#include "ps_token.h"
// #include "ps_vm.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct _ps_lexer_t
    {
        // clang-format off
        ps_buffer_t buffer;
        int      current_line;
        int      current_column;
        char     current_char;
        ps_error_t  error;
        token_t  current_token;
        // clang-format on
    } ps_lexer_t;

    void ps_lexer_init(ps_lexer_t *lexer);
    void ps_lexer_reset_cursor(ps_lexer_t *lexer);
    bool ps_lexer_read_next_char(ps_lexer_t *lexer);
    char ps_lexer_peek_char(ps_lexer_t *lexer);
    char ps_lexer_peek_next_char(ps_lexer_t *lexer);
    bool ps_lexer_read_next_token(ps_lexer_t *lexer);
    bool ps_lexer_skip_whitespace_and_comments(ps_lexer_t *lexer);
    bool ps_lexer_read_identifier_or_keyword(ps_lexer_t *lexer);
    bool ps_lexer_read_number(ps_lexer_t *lexer);

#ifdef __cplusplus
}
#endif

#endif /* _PS_LEXER_H */
