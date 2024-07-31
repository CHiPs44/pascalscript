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

#include "ps_buffer.h"
#include "ps_error.h"
#include "ps_lexer.h"
#include "ps_token.h"

ps_lexer *ps_lexer_init()
{
    ps_lexer *lexer = calloc(1, sizeof(ps_lexer));
    if (lexer == NULL)
        return false;
    lexer->buffer = ps_buffer_init();
    if (lexer->buffer == NULL)
    {
        free(lexer);
        return NULL;
    }
    ps_lexer_reset(lexer);
    return lexer;
}

void ps_lexer_done(ps_lexer *lexer)
{
    ps_buffer_done(lexer->buffer);
    free(lexer);
}

void ps_lexer_reset(ps_lexer *lexer)
{
    ps_buffer_reset(lexer->buffer);
    lexer->error = PS_LEXER_ERROR_NONE;
    lexer->current_token.type = TOKEN_NONE;
}

char *ps_lexer_show_error(ps_lexer *lexer)
{
    static char ps_lexer_error_message[256];
    snprintf(ps_lexer_error_message, 255, "LEXER: %d %s, line %d, column %d",
             lexer->error, ps_error_get_message(lexer->error),
             lexer->buffer->current_line, lexer->buffer->current_column);
    return ps_lexer_error_message;
}

bool ps_lexer_return_error(ps_lexer *lexer, ps_error error, char *message)
{
    if (message != NULL)
        fprintf(stderr, "ps_lexer_return_error: %s / %d %s\n", message, error, ps_error_get_message(error));
    lexer->current_token.type = TOKEN_NONE;
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

bool ps_lexer_skip_whitespace_and_comments(ps_lexer *lexer)
{
    bool changed1 = true;
    bool changed2 = true;
    bool changed3 = true;
    while (changed1 || changed2 || changed3)
    {
        if (!ps_lexer_skip_whitespace(lexer, &changed1))
        {
            printf("ps_lexer_skip_whitespace: error=%d\n", lexer->error);
            return false;
        }
        if (!ps_lexer_skip_comment1(lexer, &changed2))
        {
            printf("ps_lexer_skip_comment1: error=%d\n", lexer->error);
            return false;
        }
        if (!ps_lexer_skip_comment2(lexer, &changed3))
        {
            printf("ps_lexer_skip_comment2: error=%d\n", lexer->error);
            return false;
        }
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
            {
                printf(
                    "ps_lexer_read_identifier_or_keyword/ps_buffer_read_next_char: error=%d buffer->error=%d\n",
                    lexer->error, lexer->buffer->error);
                return false;
            }
            c = ps_buffer_peek_char(lexer->buffer);
            pos += 1;
            buffer[pos] = '\0';
            // printf("ps_lexer_read_identifier_or_keyword: LOOP \"%s\" ('%c')\n", buffer, c);
        } while (isalnum(c) || c == '_');
        // printf("ps_lexer_read_identifier_or_keyword: END  \"%s\" ('%c')\n", buffer, c);
        lexer->current_token.type = ps_token_is_keyword(buffer);
        strncpy(lexer->current_token.value.identifier, buffer, MAX_IDENTIFIER);
        return true;
    }
    lexer->current_token.type = TOKEN_NONE;
    lexer->error = PS_LEXER_ERROR_UNEXPECTED_CHARACTER;
    return false;
}

bool ps_lexer_read_number(ps_lexer *lexer)
{
    char buffer[PS_BUFFER_MAX_COLUMNS];
    char c = ps_buffer_peek_char(lexer->buffer);
    int pos = 0;
    int base = 10;

    // printf("ps_lexer_read_number: c=%d, isdigit=%s\n", c, isdigit(c) ? "Y" : "N");
    if (isdigit(c))
    {
        do
        {
            buffer[pos] = c;
            if (pos > 9)
                return ps_lexer_return_error(lexer, PS_LEXER_ERROR_OVERFLOW, "ps_lexer_read_number");
            if (!ps_lexer_read_next_char(lexer))
            {
                return false;
            }
            c = ps_buffer_peek_char(lexer->buffer);
            pos += 1;
        } while (isdigit(c));
        buffer[pos] = '\0';
        lexer->current_token.type = TOKEN_CARDINAL_VALUE;
        // TODO? use even better conversion from string to unsigned integer
        char *end;
        unsigned long u = strtoul(buffer, &end, base);
        if (errno == ERANGE || end == buffer || u > PS_UNSIGNED_MAX)
            return ps_lexer_return_error(lexer, PS_LEXER_ERROR_OVERFLOW, "ps_lexer_read_number");
        lexer->current_token.value.u = u;
        lexer->error = PS_LEXER_ERROR_NONE;
        return true;
    }
    return ps_lexer_return_error(lexer, PS_LEXER_ERROR_UNEXPECTED_CHARACTER, "ps_lexer_read_number");
}

bool ps_lexer_read_char_or_string_value(ps_lexer *lexer)
{
    char buffer[PS_BUFFER_MAX_COLUMNS + 1];
    char c1 = ps_buffer_peek_char(lexer->buffer);
    char c2 = ps_buffer_peek_next_char(lexer->buffer);
    int pos = 0;

    if (c1 == '\'')
    {
        if (!ps_lexer_read_next_char(lexer))
            return ps_lexer_return_error(lexer, PS_LEXER_ERROR_STRING_NOT_MULTI_LINE, "ps_lexer_read_char_or_string_value #1");
        c1 = ps_buffer_peek_char(lexer->buffer);
        c2 = ps_buffer_peek_next_char(lexer->buffer);
        while (true)
        {
            if (c1 == '\n' || c2 == '\n')
                return ps_lexer_return_error(lexer, PS_LEXER_ERROR_STRING_NOT_MULTI_LINE, "ps_lexer_read_char_or_string_value #2");
            // printf("ps_lexer_read_char_or_string_value: LOOP1 c=%d, peek=%d\n" /*, quote=%s\n"*/, c1, c2); //, quote ? "Y" : "N");
            if (pos > ps_string_max)
                return ps_lexer_return_error(lexer, PS_LEXER_ERROR_STRING_TOO_LONG, "ps_lexer_read_char_or_string_value #3");
            if (c1 == '\'' && c2 == '\'')
            {
                // printf("ps_lexer_read_char_or_string_value: quote1!\n");
                buffer[pos] = '\'';
                pos += 1;
                if (!ps_lexer_read_next_char(lexer))
                    return false;
                c1 = ps_buffer_peek_char(lexer->buffer);
                c2 = ps_buffer_peek_next_char(lexer->buffer);
                continue;
            }
            buffer[pos] = c1;
            pos += 1;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            c1 = ps_buffer_peek_char(lexer->buffer);
            c2 = ps_buffer_peek_next_char(lexer->buffer);
            // printf("ps_lexer_read_char_or_string_value: LOOP2 c=%d, peek=%d\n" /*, quote=%s\n"*/, c1, c2); //, quote ? "Y" : "N");
            if (c1 == '\'' && c2 == '\'')
            {
                // printf("ps_lexer_read_char_or_string_value: quote2!\n");
                continue;
            }
            else if (c1 == '\'')
            {
                // printf("ps_lexer_read_char_or_string_value: done!\n");
                break;
            }
        }
        buffer[pos] = '\0';
        if (!ps_lexer_read_next_char(lexer))
        {
            printf("FALSE 1 ps_lexer_read_next_char\n");
            return false;
        }
        if (pos == 1)
        {
            lexer->current_token.type = TOKEN_CHAR_VALUE;
            lexer->current_token.value.c = buffer[0];
        }
        else
        {
            lexer->current_token.type = TOKEN_STRING_VALUE;
            strcpy(lexer->current_token.value.s, buffer);
        }
        lexer->error = PS_LEXER_ERROR_NONE;
        return true;
    }
    lexer->current_token.type = TOKEN_NONE;
    lexer->error = PS_LEXER_ERROR_UNEXPECTED_CHARACTER;
    return false;
}

bool ps_lexer_read_next_token(ps_lexer *lexer)
{
    if (!ps_lexer_skip_whitespace_and_comments(lexer))
    {
        printf("ps_lexer_skip_whitespace_and_comments: error=%d\n", lexer->error);
        return false;
    }
    char current_char = ps_buffer_peek_char(lexer->buffer);
    char next_char = ps_buffer_peek_next_char(lexer->buffer);
    if (isdigit(current_char))
    {
        // printf("digit!\n");
        if (!ps_lexer_read_number(lexer))
        {
            printf("ps_lexer_read_number: error=%d\n", lexer->error);
            return false;
        }
    }
    else if (isalnum(current_char))
    {
        // printf("alnum!\n");
        if (!ps_lexer_read_identifier_or_keyword(lexer))
        {
            printf("ps_lexer_read_identifier_or_keyword: error=%d\n", lexer->error);
            return false;
        }
        lexer->current_token.type = ps_token_is_keyword(lexer->current_token.value.identifier);
        // printf("identifier: \"%s\" %d\n", lexer->current_token.value.identifier, lexer->current_token.type);
    }
    else if (current_char == '\'')
    {
        if (!ps_lexer_read_char_or_string_value(lexer))
        {
            printf("AFTER ps_lexer_read_char_or_string_value: error=%d %s\n", lexer->error, ps_error_get_message(lexer->error));
            return false;
        }
    }
    else
    {
        // printf("%c\n", c);
        sprintf(lexer->current_token.value.identifier, "%c", current_char);
        switch (current_char)
        {
        case ':':
            next_char = ps_buffer_peek_next_char(lexer->buffer);
            if (next_char == '=')
            {
                sprintf(lexer->current_token.value.identifier, ":=");
                lexer->current_token.type = TOKEN_DOT_COLON;
                ps_lexer_read_next_char(lexer);
            }
            else
                lexer->current_token.type = TOKEN_COLON;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '@':
            lexer->current_token.type = TOKEN_AT_SIGN;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '^':
            lexer->current_token.type = TOKEN_CARET;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case ',':
            lexer->current_token.type = TOKEN_COMMA;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '.':
            next_char = ps_buffer_peek_next_char(lexer->buffer);
            if (next_char == '.')
            {
                sprintf(lexer->current_token.value.identifier, "..");
                lexer->current_token.type = TOKEN_DOT_DOT;
                ps_lexer_read_next_char(lexer);
            }
            else
                lexer->current_token.type = TOKEN_DOT;
            ps_lexer_read_next_char(lexer);
            break;
        case '[':
            lexer->current_token.type = TOKEN_LEFT_BRACKET;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '(':
            lexer->current_token.type = TOKEN_LEFT_PARENTHESIS;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case ']':
            lexer->current_token.type = TOKEN_RIGHT_BRACKET;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case ')':
            lexer->current_token.type = TOKEN_RIGHT_PARENTHESIS;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case ';':
            lexer->current_token.type = TOKEN_SEMI_COLON;
            if (!ps_lexer_read_next_char(lexer))
            {
                printf("TOKEN_SEMI_COLON: error=%d\n", lexer->error);
                return false;
            }
            // ps_lexer_dump(lexer);
            break;
        case '+':
            lexer->current_token.type = TOKEN_PLUS;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '-':
            lexer->current_token.type = TOKEN_MINUS;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '*':
            lexer->current_token.type = TOKEN_STAR;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '/':
            lexer->current_token.type = TOKEN_SLASH;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '=':
            lexer->current_token.type = TOKEN_EQUAL;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '<':
            next_char = ps_buffer_peek_next_char(lexer->buffer);
            if (next_char == '>')
            {
                sprintf(lexer->current_token.value.identifier, "<>");
                lexer->current_token.type = TOKEN_NOT_EQUAL;
                if (!ps_lexer_read_next_char(lexer))
                    return false;
            }
            else if (next_char == '=')
            {
                sprintf(lexer->current_token.value.identifier, "<=");
                lexer->current_token.type = TOKEN_LESS_OR_EQUAL;
                if (!ps_lexer_read_next_char(lexer))
                    return false;
            }
            else
                lexer->current_token.type = TOKEN_LESS_THAN;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            break;
        case '>':
            next_char = ps_buffer_peek_next_char(lexer->buffer);
            if (next_char == '=')
            {
                sprintf(lexer->current_token.value.identifier, ">=");
                lexer->current_token.type = TOKEN_GREATER_OR_EQUAL;
                if (!ps_lexer_read_next_char(lexer))
                    return false;
            }
            else
                lexer->current_token.type = TOKEN_GREATER_THAN;
            ps_lexer_read_next_char(lexer);
            break;
        default:
            printf("DEFAULT! %c %d\n", current_char, current_char);
            lexer->error = PS_LEXER_ERROR_UNEXPECTED_CHARACTER;
            return false;
        }
    }
    // printf("ps_lexer_read_next_token: END\n");
    return true;
}

void ps_lexer_dump(ps_lexer *lexer)
{
    fprintf(stderr, "LEXER: token.type=%d, error=%d\n", lexer->current_token.type, lexer->error);
}

// ps_error ps_lexer_expect_token_types(lexer_t *lexer, size_t token_type_count, ps_token_type token_type[])

/* EOF */
