/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_LEXER_INTERNAL_H
#define _PS_LEXER_INTERNAL_H

#include <stdbool.h>

#include "ps_lexer.h"
#include "ps_string.h"

#define ADVANCE()                                                                                                      \
    if (!ps_lexer_read_next_char(lexer))                                                                               \
        return false;

#define GET_NEXT_CHAR(C)                                                                                               \
    {                                                                                                                  \
        ADVANCE()                                                                                                      \
        C = ps_buffer_peek_char(lexer->buffer);                                                                        \
    }

bool ps_lexer_return_error(ps_lexer *lexer, ps_error error, char *message);
bool ps_lexer_read_next_char(ps_lexer *lexer);
bool ps_lexer_skip_whitespace_and_comments(ps_lexer *lexer);
bool ps_lexer_read_number(ps_lexer *lexer);
bool ps_lexer_read_char_or_string_value(ps_lexer *lexer);

bool ps_lexer_append_char(ps_lexer *lexer, ps_string **string, char c);

#endif /* _PS_LEXER_INTERNAL_H */
