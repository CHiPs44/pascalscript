/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <errno.h>

#include "error.h"
#include "lexer.h"

// token_t yylval;
token_t *token = &yylval;

/**
 * @brief Copy current identifier into current token
 *
 * @param buffer
 * @return int LEXER_ERROR_NONE | LEXER_ERROR_IDENTIFIER_TOO_LONG
 */
error_code_t copy_identifier(const char *buffer)
{
    size_t length = strlen(buffer);
    if (length > MAX_IDENTIFIER)
    {
        return LEXER_ERROR_IDENTIFIER_TOO_LONG;
    }
    token->type = IDENTIFIER;
    strcpy(token->value.identifier, buffer);
    return LEXER_ERROR_NONE;
}

/**
 * @brief Parse current integer value into current token
 *
 * @param buffer
 * @return int  LEXER_ERROR_NONE | LEXER_ERROR_OVERFLOW
 */
error_code_t copy_integer_value(const char *buffer)
{
    long val = strtoul(buffer, 0, 10);
    // fprintf(stderr, " [copy_integer_value %s %ld %d %d]", buffer, val, errno, INT_MAX);
    if (errno == ERANGE || val > INT_MAX)
    {
        fprintf(stderr, "LEXER_ERROR_OVERFLOW %s %ld", buffer, val);
        return LEXER_ERROR_OVERFLOW;
    }
    token->type = INT_VAL;
    token->value.int_val = (int)val;
    return LEXER_ERROR_NONE;
}

/* EOF */
