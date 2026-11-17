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
        ps_buffer *buffer;      /** @brief Source                        */
        ps_error error;         /** @brief Error code                    */
        uint16_t start_line;    /** @brief Start line of current token   */
        uint16_t start_column;  /** @brief Start colmun of current token */
        ps_token current_token; /** @brief Current token itself          */
    } ps_lexer;

    /** @brief Allocate new lexer */
    ps_lexer *ps_lexer_alloc(void);

    /**
     * @brief Free lexer
     * @return NULL
     */
    ps_lexer *ps_lexer_free(ps_lexer *lexer);

    /** @brief Reset lexer to a known state */
    void ps_lexer_reset(ps_lexer *lexer);

    /** @brief Read next token from buffer */
    bool ps_lexer_read_token(ps_lexer *lexer);

    /**
     * @brief Get a debug-friendly string representation of the current token's value.
     * @return Static buffer of at most 127 chars
     */
    char *ps_lexer_get_debug_value(ps_lexer *lexer);

    /** @brief Dump the current state of the lexer. */
    void ps_lexer_dump(ps_lexer *lexer);

#ifdef __cplusplus
}
#endif

#endif /* _PS_LEXER_H */
