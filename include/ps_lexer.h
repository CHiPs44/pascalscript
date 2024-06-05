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
#include "ps_lexer.h"
#include "ps_token.h"
#include "ps_vm.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct _lexer_t
    {
        buffer_t buffer;
        int current_line;
        int current_column;
        char current_char;
        error_t error;
    } lexer_t;

    void lexer_init(lexer_t *lexer);
    extern void lexer_dump_token(token_t *token);
    // error_t lexer_read_token(lexer_t *lexer);
    error_t lexer_skip_whitespace_and_comments(lexer_t *lexer);
    bool lexer_read_identifier_or_keyword(lexer_t *lexer);
    bool lexer_read_number(lexer_t *lexer);
    

#ifdef __cplusplus
}
#endif

#endif /* _PS_LEXER_H */