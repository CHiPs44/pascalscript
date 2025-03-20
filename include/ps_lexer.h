/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_LEXER_H
#define _PS_LEXER_H

#include <stdbool.h>

// #include "ps_config.h"
// #include "ps_error.h"
#include "ps_buffer.h"
#include "ps_token.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct s_ps_lexer
    {
        ps_buffer *buffer;
        ps_error error;
        ps_token current_token;
        bool allocated;
    } ps_lexer;

    /**
     * @brief Initialize a lexer
     */
    ps_lexer *ps_lexer_init();

    /**
     * @brief Release a lexer
     */
    void ps_lexer_done(ps_lexer *lexer);
    /**
     * @brief Reset lexer:
     *  - reset "cursor" position to beginning of buffer
     *  - clear current token and error
     */
    void ps_lexer_reset(ps_lexer *lexer);
    char *ps_lexer_show_error(ps_lexer *lexer);
    bool ps_lexer_read_next_token(ps_lexer *lexer);
    bool ps_lexer_skip_whitespace_and_comments(ps_lexer *lexer);
    bool ps_lexer_read_identifier_or_keyword(ps_lexer *lexer);
    bool ps_lexer_read_number(ps_lexer *lexer);

    void ps_lexer_dump(ps_lexer *lexer);

#ifdef __cplusplus
}
#endif

#endif /* _PS_LEXER_H */
