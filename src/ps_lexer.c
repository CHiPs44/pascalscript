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

bool ps_lexer_init(ps_lexer_t *lexer)
{
    if (!ps_buffer_init(&lexer->buffer))
        return false;
    ps_lexer_reset(lexer);
    return true;
}

bool ps_lexer_done(ps_lexer_t *lexer)
{
    if (!ps_buffer_done(&lexer->buffer))
        return false;
    memset(lexer, 0, sizeof(ps_lexer_t));
    return true;
}

void ps_lexer_reset(ps_lexer_t *lexer)
{
    ps_buffer_reset(&lexer->buffer);
    lexer->error = LEXER_ERROR_NONE;
    lexer->current_token.type = TOKEN_NONE;
}

bool ps_lexer_return_error(ps_lexer_t *lexer, ps_error_t error)
{
    lexer->current_token.type = TOKEN_NONE;
    lexer->error = error;
    return false;
}

bool ps_lexer_read_next_char(ps_lexer_t *lexer)
{
    if (ps_buffer_read_next_char(&lexer->buffer))
        return true;
    return ps_lexer_return_error(lexer, lexer->buffer.error);
}

bool ps_lexer_peek_char(ps_lexer_t *lexer, char *c)
{
    if (ps_buffer_peek_char(&lexer->buffer, c))
        return true;
    return ps_lexer_return_error(lexer, lexer->buffer.error);
}

bool ps_lexer_peek_next_char(ps_lexer_t *lexer, char *c)
{
    if (ps_buffer_peek_next_char(&lexer->buffer, c))
        return true;
    return ps_lexer_return_error(lexer, lexer->buffer.error);
}

bool ps_lexer_skip_whitespace(ps_lexer_t *lexer, bool *changed)
{
    char c;
    *changed = false;
    if (!ps_lexer_peek_char(lexer, &c))
        return false;
    // printf("ps_lexer_skip_whitespace: c='%c' / %d\n", c, c);
    while (isspace(c) || c == '\n')
    {
        if (!ps_lexer_read_next_char(lexer))
            return false;
        // printf("ps_lexer_skip_whitespace: c='%c' / %d pos=%d,%d\n", c, c, lexer->buffer.current_line, lexer->buffer.current_column);
        *changed = true;
        if (!ps_lexer_peek_char(lexer, &c))
            return false;
    }
    return true;
}

bool ps_lexer_skip_comment1(ps_lexer_t *lexer, bool *changed)
{
    char c;
    *changed = false;
    if (!ps_lexer_peek_char(lexer, &c))
        return false;
    if (c == '{')
    {
        *changed = true;
        while (c != '}')
        {
            if (!ps_lexer_read_next_char(lexer))
                return false;
            c = lexer->buffer.current_char;
            if (c == '\0')
                return ps_lexer_return_error(lexer, LEXER_ERROR_UNEXPECTED_EOF);
            // printf("ps_lexer_skip_comment1: c='%c'\n", c);
        }
        if (!ps_lexer_read_next_char(lexer))
            return false;
    }
    return true;
}

bool ps_lexer_skip_comment2(ps_lexer_t *lexer, bool *changed)
{
    char c1, c2;
    *changed = false;
    if (!ps_lexer_peek_char(lexer, &c1))
    {
        printf("ps_lexer_skip_comment2: PEEK!\n");
        return false;
    }
    if (!ps_lexer_peek_next_char(lexer, &c2))
    {
        printf("ps_lexer_skip_comment2: NEXT! ('%c' %d)\n", c1, c2);
        return false;
    }
    // printf("ps_lexer_skip_comment2: BEGIN c1=%d/%d, c2=%d/%d\n", c1, '(', c2, '*');
    if (c1 == '(' && c2 == '*')
    {
        *changed = true;
        do
        {
            if (!ps_lexer_read_next_char(lexer))
                return false;
            if (!ps_lexer_peek_char(lexer, &c1))
                return false;
            if (!ps_lexer_peek_next_char(lexer, &c2))
                return false;
            // printf("ps_lexer_skip_comment2: LOOP c1=%d/%d, c2=%d/%d\n", c1, '(', c2, '*');
            if (c1 == '\0' || c2 == '\0')
                return ps_lexer_return_error(lexer, LEXER_ERROR_UNEXPECTED_EOF);
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

bool ps_lexer_skip_whitespace_and_comments(ps_lexer_t *lexer)
{
    bool changed1 = true;
    bool changed2 = true;
    bool changed3 = true;
    // printf("ps_lexer_skip_whitespace_and_comments: BEGIN\n");
    while (changed1 || changed2 || changed3)
    {
        // printf("ps_lexer_skip_whitespace_and_comments: LOOP %s %s %s\n", changed1 ? "1" : "_", changed2 ? "2" : "_", changed3 ? "3" : "_");
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
    // printf("ps_lexer_skip_whitespace_and_comments: END\n");
    return true;
}

bool ps_lexer_read_identifier_or_keyword(ps_lexer_t *lexer)
{
    char buffer[PS_BUFFER_MAX_COLUMNS];
    char c;
    int pos = 0;

    if (!ps_lexer_peek_char(lexer, &c))
    {
        printf("ps_lexer_read_identifier_or_keyword/ps_buffer_peek_char: error=%d buffer.error=%d\n", lexer->error, lexer->buffer.error);
        return false;
    }
    // printf("ps_lexer_read_identifier_or_keyword: '%c' / %d / %0x\n", c, c, c);
    if (isalpha(c))
    {
        do
        {
            // printf("%c\n", c);
            buffer[pos] = toupper(c);
            if (pos > MAX_IDENTIFIER)
            {
                return ps_lexer_return_error(lexer, LEXER_ERROR_IDENTIFIER_TOO_LONG);
            }
            if (!ps_lexer_read_next_char(lexer))
            {
                printf("ps_lexer_read_identifier_or_keyword/ps_buffer_read_next_char: error=%d buffer.error=%d\n", lexer->error, lexer->buffer.error);
                return false;
            }
            if (!ps_lexer_peek_char(lexer, &c))
            {
                printf("ps_lexer_read_identifier_or_keyword/ps_buffer_peek_char: error=%d buffer.error=%d\n", lexer->error, lexer->buffer.error);
                return false;
            }
            pos += 1;
            buffer[pos] = '\0';
            // printf("ps_lexer_read_identifier_or_keyword: LOOP \"%s\" ('%c')\n", buffer, c);
        } while (isalnum(c) || c == '_');
        buffer[pos] = '\0';
        // printf("ps_lexer_read_identifier_or_keyword: END  \"%s\" ('%c')\n", buffer, c);
        lexer->current_token.type = ps_token_is_keyword(buffer);
        strncpy(lexer->current_token.value.identifier, buffer, MAX_IDENTIFIER);
        return true;
    }
    lexer->current_token.type = TOKEN_NONE;
    lexer->error = LEXER_ERROR_UNEXPECTED_CHARACTER;
    return false;
}

bool ps_lexer_read_number(ps_lexer_t *lexer)
{
    char buffer[PS_BUFFER_MAX_COLUMNS];
    char c;
    int pos = 0;
    int base = 10;

    if (!ps_lexer_peek_char(lexer, &c))
        return false;
    printf("ps_lexer_read_number: c=%d, isdigit=%s\n", c, isdigit(c) ? "Y" : "N");
    if (isdigit(c))
    {
        do
        {
            buffer[pos] = c;
            if (pos > 9)
                return ps_lexer_return_error(lexer, LEXER_ERROR_OVERFLOW);
            if (!ps_lexer_read_next_char(lexer))
            {
                return false;
            }
            if (!ps_lexer_peek_char(lexer, &c))
                return false;
            pos += 1;
        } while (isdigit(c));
        buffer[pos] = '\0';
        lexer->current_token.type = TOKEN_CARDINAL_VALUE;
        // TODO use better conversion from string to unsigned integer
        lexer->current_token.value.u = strtoul(buffer, NULL, base);
        lexer->error = LEXER_ERROR_NONE;
        return true;
    }
    return ps_lexer_return_error(lexer, LEXER_ERROR_UNEXPECTED_CHARACTER);
}

bool ps_lexer_read_char_or_string_value(ps_lexer_t *lexer)
{
    char buffer[PS_BUFFER_MAX_COLUMNS + 1];
    char c1, c2;
    int pos = 0;

    if (!ps_lexer_peek_char(lexer, &c1))
        return false;
    if (c1 == '\'')
    {
        // if (!ps_lexer_read_next_char(lexer))
        //     return false;
        // if (!ps_lexer_peek_char(lexer, &c1))
        //     return false;
        do
        {
            if (!ps_lexer_peek_next_char(lexer, &c2))
                return false;
            if (c2 == '\n')
                return ps_lexer_return_error(lexer, LEXER_ERROR_STRING_NOT_MULTI_LINE);
            printf("ps_lexer_read_char_or_string_value: LOOP1 c=%d, peek=%d\n" /*, quote=%s\n"*/, c1, c2); //, quote ? "Y" : "N");
            if (c1 == '\'' && c2 == '\'')
            {
                printf("ps_lexer_read_char_or_string_value: quote!\n");
                buffer[pos] = '\'';
                pos += 1;
                // TODO check pos
                if (!ps_lexer_read_next_char(lexer))
                    return false;
                if (!ps_lexer_peek_char(lexer, &c1))
                    return false;
                continue;
            }
            if (pos > ps_string_max)
                return ps_lexer_return_error(lexer, LEXER_ERROR_STRING_TOO_LONG);
            buffer[pos] = c1;
            pos += 1;
            if (!ps_lexer_read_next_char(lexer))
                return false;
            if (!ps_lexer_peek_char(lexer, &c1))
                return false;
            if (!ps_lexer_peek_next_char(lexer, &c2))
                return false;
            printf("ps_lexer_read_char_or_string_value: LOOP2 c=%d, peek=%d\n" /*, quote=%s\n"*/, c1, c2); //, quote ? "Y" : "N");
        } while (c1 != '\'' && c2 != '\'');
        buffer[pos] = '\0';
        if (!ps_lexer_read_next_char(lexer))
            return false;
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
        lexer->error = LEXER_ERROR_NONE;
        return true;
    }
    lexer->current_token.type = TOKEN_NONE;
    lexer->error = LEXER_ERROR_UNEXPECTED_CHARACTER;
    return false;
}

bool ps_lexer_read_next_token(ps_lexer_t *lexer)
{
    char c1, c2;

    if (!ps_lexer_skip_whitespace_and_comments(lexer))
    {
        printf("ps_lexer_skip_whitespace_and_comments: error=%d\n", lexer->error);
        return false;
    }
    if (!ps_lexer_peek_char(lexer, &c1))
    {
        printf("ps_buffer_peek_char: error=%d\n", lexer->error);
        return false;
    }
    if (isdigit(c1))
    {
        // printf("digit!\n");
        if (!ps_lexer_read_number(lexer))
        {
            printf("ps_lexer_read_number: error=%d\n", lexer->error);
            return false;
        }
    }
    else if (isalnum(c1))
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
    else if (c1 == '\'')
    {
        if (!ps_lexer_read_char_or_string_value(lexer))
        {
            printf("AFTER ps_lexer_read_char_or_string_value: error=%d\n", lexer->error);
            return false;
        }
    }
    else
    {
        // printf("%c\n", c);
        sprintf(lexer->current_token.value.identifier, "%c", c1);
        switch (c1)
        {
        case ':':
            if (!ps_lexer_peek_next_char(lexer, &c2))
                return false;
            if (c2 == '=')
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
            if (!ps_lexer_peek_next_char(lexer, &c2))
                return false;
            if (c2 == '.')
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
            if (!ps_lexer_peek_next_char(lexer, &c2))
                return false;
            if (c2 == '>')
            {
                sprintf(lexer->current_token.value.identifier, "<>");
                lexer->current_token.type = TOKEN_NOT_EQUAL;
                if (!ps_lexer_read_next_char(lexer))
                    return false;
            }
            else if (c2 == '=')
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
            if (!ps_lexer_peek_next_char(lexer, &c2))
                return false;
            if (c2 == '=')
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
            lexer->error = LEXER_ERROR_UNEXPECTED_CHARACTER;
            return false;
        }
    }
    // printf("ps_lexer_read_next_token: END\n");
    return true;
}

void ps_lexer_dump(ps_lexer_t *lexer)
{
    fprintf(stderr, "LEXER: token.type=%d, error=%d\n", lexer->current_token.type, lexer->error);
}

// ps_error_t ps_lexer_expect_token_types(lexer_t *lexer, size_t token_type_count, ps_token_type_t token_type[])

/* EOF */
