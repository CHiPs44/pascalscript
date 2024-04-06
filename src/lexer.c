/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "lexer.h"
#include "source.h"
#include "symbol.h"

error_t lexer_read_token(vm_t *vm, token_t *token)
{
    char buffer[MAX_COLUMNS];
    char c;
    int pos = 0;

    c = source_peek_char(vm);
    if (c == '\0')
    {
        token->type = TOKEN_END_OF_FILE;
        return ERROR_NONE;
    }
    // skip whitespace
    while (isspace(c))
    {
        c = source_read_char(vm);
        if (c == '\0')
        {
            token->type = TOKEN_NONE;
            return LEXER_ERROR_UNEXPECTED_EOF;
        }
    }
    if (isalpha(c))
    {
        do
        {
            buffer[pos] = toupper(c);
            pos += 1;
            if (pos > MAX_IDENTIFIER)
            {
                token->type = TOKEN_NONE;
                return LEXER_ERROR_IDENTIFIER_TOO_LONG;
            }
            buffer[pos] = '\0';
            c = source_read_char(vm);
        } while (isalnum(c));
        // TODO keywords
        token->type = TOKEN_IDENTIFIER;
        strcpy(token->value.identifier, buffer);
        return ERROR_NONE;
    }
    else if (isdigit(c))
    {
        do
        {
            buffer[pos] = c;
            pos += 1;
            if (pos > 12)
            {
                token->type = TOKEN_NONE;
                return LEXER_ERROR_OVERFLOW;
            }
            buffer[pos] = '\0';
            c = source_read_char(vm);
        } while (isdigit(c));
        if (c == '\0')
        {
            token->type = TOKEN_NONE;
            return LEXER_ERROR_UNEXPECTED_EOF;
        }
    }
    else if (c == '+')
    {
        token->type = TOKEN_ADD;
        return ERROR_NONE;
    }
    else if (c == '-')
    {
        token->type = TOKEN_SUB;
        return ERROR_NONE;
    }
    else if (c == '*')
    {
        token->type = TOKEN_MUL;
        return ERROR_NONE;
    }
    else if (c == '/')
    {
        token->type = TOKEN_DIV;
        return ERROR_NONE;
    }
    return LEXER_ERROR_UNEXPECTED_CHARACTER;
}

void lexer_dump_token(token_t *token)
{
    char *type;
    char value[256];

    switch (token->type)
    {
    case TOKEN_IDENTIFIER:
        type = "IDENTIFIER";
        snprintf(value, 256, "%s", token->value.identifier);
        break;
    case TOKEN_INTEGER_VALUE:
        type = "INTEGER";
        snprintf(value, 256, "%d", token->value.int_val);
        break;
    case TOKEN_REAL_VALUE:
        type = "REAL";
        snprintf(value, 256, "%f", token->value.real_val);
        break;
    case TOKEN_CHAR_VALUE:
        type = "CHAR";
        snprintf(value, 256, "%c", token->value.char_val);
        break;
    case TOKEN_STRING_VALUE:
        type = "STRING";
        snprintf(value, 256, "%s", token->value.string_val);
        break;
    default:
        break;
    }
    fprintf(stderr, "TOKEN: type=%s, value=%s\n", type, value);
}

/* EOF */
