/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_LEXER_H
#define _PS_LEXER_H

#include <stdbool.h>

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
    } ps_lexer;

    ps_lexer *ps_lexer_init();
    ps_lexer *ps_lexer_done(ps_lexer *lexer);
    void ps_lexer_reset(ps_lexer *lexer);
    char *ps_lexer_show_error(ps_lexer *lexer);
    bool ps_lexer_read_token(ps_lexer *lexer);
    bool ps_lexer_get_cursor(ps_lexer *lexer, uint16_t *line, uint16_t *column);
    bool ps_lexer_set_cursor(ps_lexer *lexer, uint16_t line, uint16_t column);

    char *ps_lexer_get_debug_value(ps_lexer *lexer);
    void ps_lexer_dump(ps_lexer *lexer);

#ifdef __cplusplus
}
#endif

#endif /* _PS_LEXER_H */
