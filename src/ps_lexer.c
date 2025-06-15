/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "ps_buffer.h"
#include "ps_error.h"
#include "ps_lexer.h"
#include "ps_token.h"

ps_lexer *ps_lexer_init(ps_lexer *lexer)
{
    if (lexer == NULL)
    {
        lexer = calloc(1, sizeof(ps_lexer));
        if (lexer == NULL)
            return NULL;
        lexer->allocated = true;
    }
    else
    {
        lexer->allocated = false;
    }
    lexer->buffer = ps_buffer_init();
    if (lexer->buffer == NULL)
    {
        ps_lexer_done(lexer);
        return NULL;
    }
    ps_lexer_reset(lexer);
    return lexer;
}

void ps_lexer_done(ps_lexer *lexer)
{
    if (lexer->buffer != NULL)
        ps_buffer_done(lexer->buffer);
    if (!lexer->allocated)
        return;
    free(lexer);
}

void ps_lexer_reset(ps_lexer *lexer)
{
    ps_buffer_reset(lexer->buffer);
    lexer->error = PS_LEXER_ERROR_NONE;
    lexer->current_token.type = PS_TOKEN_NONE;
}

char *ps_lexer_show_error(ps_lexer *lexer)
{
    static char error_message[128];
    snprintf(error_message, sizeof(error_message) - 1,
             "LEXER: %d %s, line %d, column %d",
             lexer->error, ps_error_get_message(lexer->error),
             lexer->buffer->current_line, lexer->buffer->current_column);
    return error_message;
}

bool ps_lexer_return_error(ps_lexer *lexer, ps_error error, char *message)
{
    if (message != NULL)
        fprintf(stderr, "%s: %d %s at line %d column %d %c\n",
                message, error, ps_error_get_message(error),
                lexer->buffer->current_line, lexer->buffer->current_column,
                lexer->buffer->current_char >= ' ' ? lexer->buffer->current_char : '_');
    lexer->current_token.type = PS_TOKEN_NONE;
    lexer->error = error;
    return false;
}

bool ps_lexer_read_next_char(ps_lexer *lexer)
{
    if (ps_buffer_read_next_char(lexer->buffer))
        return true;
    return ps_lexer_return_error(lexer, lexer->buffer->error, "ps_lexer_read_next_char");
}

bool ps_lexer_skip_whitespace(ps_lexer *lexer, bool *changed)
{
    char c = ps_buffer_peek_char(lexer->buffer);
    *changed = false;
    while (isspace(c) || c == '\n')
    {
        if (!ps_lexer_read_next_char(lexer))
            return false;
        *changed = true;
        c = ps_buffer_peek_char(lexer->buffer);
    }
    return true;
}

bool ps_lexer_skip_comment1(ps_lexer *lexer, bool *changed)
{
    char c = ps_buffer_peek_char(lexer->buffer);
    *changed = false;
    if (c == '{')
    {
        *changed = true;
        while (c != '}')
        {
            if (!ps_lexer_read_next_char(lexer))
                return false;
            c = lexer->buffer->current_char;
            if (c == '\0')
                return ps_lexer_return_error(lexer, PS_LEXER_ERROR_UNEXPECTED_EOF, "ps_lexer_skip_comment1");
        }
        if (!ps_lexer_read_next_char(lexer))
            return false;
    }
    return true;
}

bool ps_lexer_skip_comment2(ps_lexer *lexer, bool *changed)
{
    char c1 = ps_buffer_peek_char(lexer->buffer);
    char c2 = ps_buffer_peek_next_char(lexer->buffer);
    *changed = false;
    if (c1 == '(' && c2 == '*')
    {
        *changed = true;
        do
        {
            if (!ps_lexer_read_next_char(lexer))
                return false;
            c1 = ps_buffer_peek_char(lexer->buffer);
            c2 = ps_buffer_peek_next_char(lexer->buffer);
            if (c1 == '\0' || c2 == '\0')
                return ps_lexer_return_error(lexer, PS_LEXER_ERROR_UNEXPECTED_EOF, "ps_lexer_skip_comment2");
            if (c1 == '*' && c2 == ')')
            {
                break;
            }
        } while (true);
        // Skip )
        if (!ps_lexer_read_next_char(lexer))
            return false;
        // Advance after )
        if (!ps_lexer_read_next_char(lexer))
            return false;
    }
    return true;
}

bool ps_lexer_skip_comment3(ps_lexer *lexer, bool *changed)
{
    char c1 = ps_buffer_peek_char(lexer->buffer);
    char c2 = ps_buffer_peek_next_char(lexer->buffer);
    *changed = false;
    if (c1 == '/' && c2 == '/')
    {
        *changed = true;
        while (c1 != '\n')
        {
            if (!ps_lexer_read_next_char(lexer))
                return false;
            c1 = lexer->buffer->current_char;
            if (c1 == '\0')
                return ps_lexer_return_error(lexer, PS_LEXER_ERROR_UNEXPECTED_EOF, "ps_lexer_skip_comment3");
        }
        if (!ps_lexer_read_next_char(lexer))
            return false;
    }
    return true;
}

bool ps_lexer_skip_whitespace_and_comments(ps_lexer *lexer)
{
    bool changed1 = true;
    bool changed2 = true;
    bool changed3 = true;
    bool changed4 = true;
    while (changed1 || changed2 || changed3 || changed4)
    {
        if (!ps_lexer_skip_whitespace(lexer, &changed1))
            return false;
        if (!ps_lexer_skip_comment1(lexer, &changed2))
            return false;
        if (!ps_lexer_skip_comment2(lexer, &changed3))
            return false;
        if (!ps_lexer_skip_comment3(lexer, &changed4))
            return false;
    }
    return true;
}

bool ps_lexer_read_identifier_or_keyword(ps_lexer *lexer)
{
    char buffer[PS_BUFFER_MAX_COLUMNS];
    char c = ps_buffer_peek_char(lexer->buffer);
    int pos = 0;
    if (isalpha(c))
    {
        do
        {
            if (pos > MAX_IDENTIFIER)
            {
                return ps_lexer_return_error(lexer, PS_LEXER_ERROR_IDENTIFIER_TOO_LONG, "ps_lexer_read_identifier_or_keyword");
            }
            buffer[pos] = toupper(c);
            if (!ps_lexer_read_next_char(lexer))
                return false;
            c = ps_buffer_peek_char(lexer->buffer);
            pos += 1;
            buffer[pos] = '\0';
        } while (isalnum(c) || c == '_');
        lexer->current_token.type = ps_token_is_keyword(buffer);
        strncpy(lexer->current_token.value.identifier, buffer, MAX_IDENTIFIER);
        return true;
    }
    lexer->current_token.type = PS_TOKEN_NONE;
    return ps_lexer_return_error(lexer, PS_LEXER_ERROR_UNEXPECTED_CHARACTER, "ps_lexer_read_identifier_or_keyword");
}

bool ps_lexer_read_number(ps_lexer *lexer)
{
    char buffer[PS_BUFFER_MAX_COLUMNS + 1];
    char c = ps_buffer_peek_char(lexer->buffer);
    int pos = 0;
    int base = 10;
    bool is_real = false;
    bool has_exp = false;
    char *digits = "0123456789";
    if (c == '%')
    {
        base = 2;
        digits = "01";
        if (!ps_lexer_read_next_char(lexer))
            return false;
        c = ps_buffer_peek_char(lexer->buffer);
    }
    else if (c == '&')
    {
        base = 8;
        digits = "01234567";
        if (!ps_lexer_read_next_char(lexer))
            return false;
        c = ps_buffer_peek_char(lexer->buffer);
    }
    else if (c == '$')
    {
        base = 16;
        digits = "01234567abcdefABCDEF";
        if (!ps_lexer_read_next_char(lexer))
            return false;
        c = ps_buffer_peek_char(lexer->buffer);
    }
    if (strchr(digits, c) != NULL)
    {
        do
        {
            if (pos > PS_BUFFER_MAX_COLUMNS)
                return ps_lexer_return_error(lexer, PS_LEXER_ERROR_OVERFLOW, "ps_lexer_read_number");
            buffer[pos++] = c;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            c = ps_buffer_peek_char(lexer->buffer);
            if (base == 10)
            {
                if (c == '.' && !is_real)
                {
                    is_real = true;
                    buffer[pos++] = c;
                    if (!ps_lexer_read_next_char(lexer))
                        return false;
                    c = ps_buffer_peek_char(lexer->buffer);
                }
                else if ((c == 'e' || c == 'E'))
                {
                    if (has_exp)
                        return ps_lexer_return_error(lexer, PS_LEXER_ERROR_UNEXPECTED_CHARACTER, "ps_lexer_read_number");
                    has_exp = true;
                    is_real = true;
                    buffer[pos++] = c;
                    if (!ps_lexer_read_next_char(lexer))
                        return false;
                    c = ps_buffer_peek_char(lexer->buffer);
                    if (c == '+' || c == '-')
                    {
                        buffer[pos++] = c;
                        if (!ps_lexer_read_next_char(lexer))
                            return false;
                        c = ps_buffer_peek_char(lexer->buffer);
                    }
                }
            }
        } while (strchr(digits, c) != NULL || strchr(".eE+-", c) != NULL);
        buffer[pos] = '\0';
        // TODO? use even better conversion from string to real or unsigned integer
        char *end;
        if (is_real)
        {
            double d = strtod(buffer, &end);
            if (errno == ERANGE || end == buffer || d > PS_REAL_MAX)
                return ps_lexer_return_error(lexer, PS_LEXER_ERROR_OVERFLOW, "ps_lexer_read_number");
            lexer->current_token.type = PS_TOKEN_REAL_VALUE;
            lexer->current_token.value.r = (ps_real)d;
        }
        else
        {
            unsigned long u = strtoul(buffer, &end, base);
            if (errno == ERANGE || end == buffer || u > PS_UNSIGNED_MAX)
                return ps_lexer_return_error(lexer, PS_LEXER_ERROR_OVERFLOW, "ps_lexer_read_number");
            if (u > PS_INTEGER_MAX)
            {
                lexer->current_token.type = PS_TOKEN_UNSIGNED_VALUE;
                lexer->current_token.value.u = (ps_unsigned)u;
            }
            else
            {
                lexer->current_token.type = PS_TOKEN_INTEGER_VALUE;
                lexer->current_token.value.i = (ps_integer)u;
            }
        }
        lexer->error = PS_LEXER_ERROR_NONE;
        return true;
    }
    return ps_lexer_return_error(lexer, PS_LEXER_ERROR_UNEXPECTED_CHARACTER, "ps_lexer_read_number");
}

bool ps_lexer_read_char_or_string_value(ps_lexer *lexer)
{
    char c;
    char buffer[PS_STRING_MAX_LEN + 1];
    int pos = 0;

    c = ps_buffer_peek_char(lexer->buffer);
    if (c != '\'')
        return ps_lexer_return_error(lexer, PS_LEXER_ERROR_UNEXPECTED_CHARACTER, "ps_lexer_read_char_or_string_value");
    // Consume the opening quote
    if (!ps_lexer_read_next_char(lexer))
        return false;
    while (true)
    {
        c = ps_buffer_peek_char(lexer->buffer);
        if (c == '\0')
            return ps_lexer_return_error(lexer, PS_LEXER_ERROR_UNEXPECTED_EOF, "ps_lexer_read_char_or_string_value");
        if (c == '\'')
        {
            // Check for doubled single quotes (escaped single quote)
            char next_char = ps_buffer_peek_next_char(lexer->buffer);
            if (next_char == '\'')
            {
                if (pos >= PS_STRING_MAX_LEN)
                    return ps_lexer_return_error(lexer, PS_LEXER_ERROR_OVERFLOW, "ps_lexer_read_char_or_string_value");

                buffer[pos++] = '\'';
                if (!ps_lexer_read_next_char(lexer)) // Consume the first quote
                    return false;
                if (!ps_lexer_read_next_char(lexer)) // Consume the second quote
                    return false;
            }
            else
            {
                // End of the string
                if (!ps_lexer_read_next_char(lexer)) // Consume the closing quote
                    return false;
                break;
            }
        }
        else
        {
            if (pos >= PS_STRING_MAX_LEN)
                return ps_lexer_return_error(lexer, PS_LEXER_ERROR_OVERFLOW, "ps_lexer_read_char_or_string_value");

            buffer[pos++] = c;
            if (!ps_lexer_read_next_char(lexer))
                return false;
        }
    }
    buffer[pos] = '\0';
    if (pos == 1)
    {
        lexer->current_token.type = PS_TOKEN_CHAR_VALUE;
        lexer->current_token.value.c = buffer[0];
    }
    else
    {
        lexer->current_token.type = PS_TOKEN_STRING_VALUE;
        strncpy((char *)lexer->current_token.value.s, buffer, PS_STRING_MAX_LEN);
    }

    lexer->error = PS_LEXER_ERROR_NONE;
    return true;
}

bool ps_lexer_read_next_token(ps_lexer *lexer)
{
    if (!ps_lexer_skip_whitespace_and_comments(lexer))
        return false;
    char current_char = ps_buffer_peek_char(lexer->buffer);
    char next_char = ps_buffer_peek_next_char(lexer->buffer);
    // fprintf(stderr, "char=%d next=%d length=%d error=%d\n", current_char, next_char, lexer->buffer->length, lexer->buffer->error);
    if (isdigit(current_char) || current_char == '%' || current_char == '&' || current_char == '$')
    {
        if (!ps_lexer_read_number(lexer))
            return false;
    }
    else if (isalnum(current_char))
    {
        if (!ps_lexer_read_identifier_or_keyword(lexer))
            return false;
        lexer->current_token.type = ps_token_is_keyword(lexer->current_token.value.identifier);
    }
    else if (current_char == '\'')
    {
        if (!ps_lexer_read_char_or_string_value(lexer))
            // if (!ps_lexer_read_char_value(lexer))
            return false;
    }
    else
    {
        // printf("%c\n", c);
        sprintf(lexer->current_token.value.identifier, "%c", current_char);
        switch (current_char)
        {
        case ':':
            if (next_char == '=')
            {
                sprintf(lexer->current_token.value.identifier, ":=");
                lexer->current_token.type = PS_TOKEN_ASSIGN;
                if (!ps_lexer_read_next_char(lexer))
                    return false;
            }
            else
                lexer->current_token.type = PS_TOKEN_COLON;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '@':
            lexer->current_token.type = PS_TOKEN_AT_SIGN;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '^':
            lexer->current_token.type = PS_TOKEN_CARET;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case ',':
            lexer->current_token.type = PS_TOKEN_COMMA;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '.':
            next_char = ps_buffer_peek_next_char(lexer->buffer);
            if (next_char == '.')
            {
                sprintf(lexer->current_token.value.identifier, "..");
                lexer->current_token.type = PS_TOKEN_RANGE;
                if (!ps_lexer_read_next_char(lexer))
                    return false;
            }
            else
                lexer->current_token.type = PS_TOKEN_DOT;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '(':
            lexer->current_token.type = PS_TOKEN_LEFT_PARENTHESIS;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case ')':
            lexer->current_token.type = PS_TOKEN_RIGHT_PARENTHESIS;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '[':
            lexer->current_token.type = PS_TOKEN_LEFT_BRACKET;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case ']':
            lexer->current_token.type = PS_TOKEN_RIGHT_BRACKET;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case ';':
            lexer->current_token.type = PS_TOKEN_SEMI_COLON;
            if (!ps_lexer_read_next_char(lexer))
            {
                printf("TOKEN_SEMI_COLON: error=%d\n", lexer->error);
                return false;
            }
            // ps_lexer_dump(lexer);
            break;
        case '+':
            lexer->current_token.type = PS_TOKEN_PLUS;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '-':
            lexer->current_token.type = PS_TOKEN_MINUS;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '*':
            if (next_char == '*')
            {
                sprintf(lexer->current_token.value.identifier, "**");
                lexer->current_token.type = PS_TOKEN_POWER;
                if (!ps_lexer_read_next_char(lexer))
                    return false;
            }
            else
                lexer->current_token.type = PS_TOKEN_STAR;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '/':
            lexer->current_token.type = PS_TOKEN_SLASH;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '=':
            lexer->current_token.type = PS_TOKEN_EQUAL;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '<':
            if (next_char == '>')
            {
                sprintf(lexer->current_token.value.identifier, "<>");
                lexer->current_token.type = PS_TOKEN_NOT_EQUAL;
                if (!ps_lexer_read_next_char(lexer))
                    return false;
            }
            else if (next_char == '=')
            {
                sprintf(lexer->current_token.value.identifier, "<=");
                lexer->current_token.type = PS_TOKEN_LESS_OR_EQUAL;
                if (!ps_lexer_read_next_char(lexer))
                    return false;
            }
            else
                lexer->current_token.type = PS_TOKEN_LESS_THAN;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '>':
            if (next_char == '=')
            {
                sprintf(lexer->current_token.value.identifier, ">=");
                lexer->current_token.type = PS_TOKEN_GREATER_OR_EQUAL;
                if (!ps_lexer_read_next_char(lexer))
                    return false;
            }
            else
                lexer->current_token.type = PS_TOKEN_GREATER_THAN;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        default:
            printf("DEFAULT! %c %d at line %d column %d\n",
                   current_char, current_char,
                   lexer->buffer->current_line, lexer->buffer->current_column);
            lexer->error = PS_LEXER_ERROR_UNEXPECTED_CHARACTER;
            return false;
        }
    }
    return true;
}

void ps_lexer_dump(ps_lexer *lexer)
{
    fprintf(stderr, "LEXER: token.type=%d, error=%d\n", lexer->current_token.type, lexer->error);
}

/* EOF */
