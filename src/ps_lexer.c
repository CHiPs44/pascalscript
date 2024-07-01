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

#include "ps_error.h"
#include "ps_buffer.h"
#include "ps_token.h"
#include "ps_lexer.h"

void ps_lexer_init(ps_lexer_t *lexer)
{
    ps_buffer_init(&lexer->buffer);
    ps_lexer_reset_cursor(lexer);
    lexer->error = LEXER_ERROR_NONE;
    lexer->current_token.type = TOKEN_NONE;
}

void ps_lexer_reset_cursor(ps_lexer_t *lexer)
{
    lexer->current_line = 0;
    lexer->current_column = 0;
    lexer->current_char = '\0';
}

char ps_lexer_peek_char(ps_lexer_t *lexer)
{
    // printf("lexer_peek_char: char=%c alpha=%d, alnum=%d, digit=%d\n", lexer->current_char, isalpha(lexer->current_char), isalnum(lexer->current_char), isdigit(lexer->current_char));
    return lexer->current_char;
}

char ps_lexer_peek_next_char(ps_lexer_t *lexer)
{
    char next_char = '\0';
    if (lexer->current_line >= 0 && lexer->current_line < lexer->buffer.line_count)
    {
        if (lexer->current_column + 1 >= 0 && lexer->current_column + 1 <= lexer->buffer.line_lengths[lexer->current_line])
        {
            next_char = lexer->buffer.line_starts[lexer->current_line][lexer->current_column + 1];
        }
    }
    return next_char;
}

bool ps_lexer_read_next_char(ps_lexer_t *lexer)
{
    // printf("ps_lexer_read_next_char: line=%d, col=%d\n", lexer->current_line, lexer->current_column);
    lexer->current_char = '\0';
    if (lexer->current_line >= 0 && lexer->current_line < lexer->buffer.line_count)
    {
        if (lexer->current_column >= 0 &&
            lexer->current_column <= lexer->buffer.line_lengths[lexer->current_line])
        {
            lexer->current_char = lexer->buffer.line_starts[lexer->current_line][lexer->current_column];
            // Advance to next char
            lexer->current_column += 1;
            if (lexer->current_column > lexer->buffer.line_lengths[lexer->current_line])
            {
                lexer->current_line += 1;
                lexer->current_column = 0;
                if (lexer->current_line > lexer->buffer.line_count)
                {
                    lexer->error = LEXER_ERROR_UNEXPECTED_EOF;
                    lexer->current_char = '\0';
                }
            }
        }
    }
    // printf("ps_lexer_read_next_char: line=%05d col=%03d char=%c\n", lexer->current_line, lexer->current_column, lexer->current_char);
    return true;
}

bool ps_lexer_return_error(ps_lexer_t *lexer, ps_error_t error)
{
    lexer->current_token.type = TOKEN_NONE;
    lexer->error = error;
    return false;
}

bool ps_lexer_skip_whitespace(ps_lexer_t *lexer, bool *changed)
{
    char c;
    c = ps_lexer_peek_char(lexer);
    // printf("ps_lexer_skip_whitespace: c='%c' / %d\n", c, c);
    *changed = false;
    while (isspace(c) || c == '\n')
    {
        c = ps_lexer_read_next_char(lexer);
        // printf("ps_lexer_skip_whitespace: c='%c' / %d %d\n", c, c, '\n');
        if (lexer->error != LEXER_ERROR_NONE)
            return false;
        *changed = true;
    }
    return true;
}

bool ps_lexer_skip_comment1(ps_lexer_t *lexer, bool *changed)
{
    char c;
    c = ps_lexer_peek_char(lexer);
    *changed = false;
    if (c == '{')
    {
        *changed = true;
        while (c != '}')
        {
            c = ps_lexer_read_next_char(lexer);
            if (lexer->error != LEXER_ERROR_NONE)
                return false;
            if (c == '\0')
                return ps_lexer_return_error(lexer, LEXER_ERROR_UNEXPECTED_EOF);
            // printf("ps_lexer_skip_comment1: c='%c'\n", c);
        }
        ps_lexer_read_next_char(lexer);
        if (lexer->error != LEXER_ERROR_NONE)
            return false;
    }
    return true;
}

bool ps_lexer_skip_comment2(ps_lexer_t *lexer, bool *changed)
{
    char c1, c2;
    c1 = ps_lexer_peek_char(lexer);
    c2 = ps_lexer_peek_next_char(lexer);
    if (lexer->error != LEXER_ERROR_NONE)
        return false;
    *changed = false;
    if (c1 == '(' && c2 == '*')
    {
        *changed = true;
        do
        {
            c1 = ps_lexer_read_next_char(lexer);
            c2 = ps_lexer_peek_next_char(lexer);
            if (c1 == '\0' || c2 == '\0')
                return ps_lexer_return_error(lexer, LEXER_ERROR_UNEXPECTED_EOF);
            if (c1 == '*' && c2 == ')')
            {
                break;
            }
        } while (true);
        // Skip )
        ps_lexer_read_next_char(lexer);
        if (lexer->error != LEXER_ERROR_NONE)
            return false;
        // Advance after )
        ps_lexer_read_next_char(lexer);
        if (lexer->error != LEXER_ERROR_NONE)
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
        ps_lexer_skip_whitespace(lexer, &changed1);
        if (lexer->error != LEXER_ERROR_NONE)
            return false;
        ps_lexer_skip_comment1(lexer, &changed2);
        if (lexer->error != LEXER_ERROR_NONE)
            return false;
        ps_lexer_skip_comment2(lexer, &changed3);
        if (lexer->error != LEXER_ERROR_NONE)
            return false;
    }
    // printf("ps_lexer_skip_whitespace_and_comments: END\n");
    return true;
}

bool ps_lexer_read_identifier_or_keyword(ps_lexer_t *lexer)
{
    // printf("ps_lexer_read_identifier_or_keyword\n");
    char buffer[PS_BUFFER_MAX_COLUMNS];
    char c;
    int pos = 0;

    c = ps_lexer_peek_char(lexer);
    if (lexer->error != LEXER_ERROR_NONE)
        return false;
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
            c = ps_lexer_read_next_char(lexer);
            if (lexer->error != LEXER_ERROR_NONE)
                return false;
            pos += 1;
        } while (isalnum(c));
        buffer[pos] = '\0';
        // printf("ps_lexer_read_identifier_or_keyword: \"%s\" ('%c')\n", buffer, c);
        lexer->current_token.type = ps_token_is_keyword(buffer);
        strncpy(lexer->current_token.value.identifier, buffer, MAX_IDENTIFIER);
        return true;
    }
    lexer->current_token.type = TOKEN_NONE;
    return false;
}

bool ps_lexer_read_number(ps_lexer_t *lexer)
{
    char buffer[PS_BUFFER_MAX_COLUMNS];
    char c;
    int pos = 0;
    int base = 10;

    c = ps_lexer_peek_char(lexer);
    if (isdigit(c))
    {
        do
        {
            buffer[pos] = c;
            if (pos > 9)
                return ps_lexer_return_error(lexer, LEXER_ERROR_OVERFLOW);
            c = ps_lexer_read_next_char(lexer);
            if (lexer->error != LEXER_ERROR_NONE)
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
    char buffer[PS_BUFFER_MAX_COLUMNS];
    char c;
    int pos = 0;
    bool quote = false;

    c = ps_lexer_peek_char(lexer);
    if (c == '\'')
    {
        c = ps_lexer_read_next_char(lexer);
        if (lexer->error != LEXER_ERROR_NONE)
            return false;
        do
        {
            if (c == '\n')
                return ps_lexer_return_error(lexer, LEXER_ERROR_STRING_NOT_MULTI_LINE);
            char c2 = ps_lexer_peek_next_char(lexer);
            printf("ps_lexer_read_char_or_string_value: c=%d, peek=%d, quote=%s\n", c, c2, quote ? "Y" : "N");
            if (c == '\'' && c2 == '\'')
            {
                printf("ps_lexer_read_char_or_string_value: quote!\n");
                quote = true;
                ps_lexer_read_next_char(lexer);
            }
            if (pos > ps_string_max)
                return ps_lexer_return_error(lexer, LEXER_ERROR_STRING_TOO_LONG);
            if (quote)
            {
                buffer[pos] = '\'';
            }
            else
            {
                buffer[pos] = c;
            }
            pos += 1;
            c = ps_lexer_read_next_char(lexer);
            if (lexer->error != LEXER_ERROR_NONE)
                return false;
            if (quote)
            {
                quote = false;
                continue;
            }
        } while (c != '\'' && ps_lexer_peek_next_char(lexer) != '\'');
        buffer[pos] = '\0';
        ps_lexer_read_next_char(lexer);
        if (lexer->error != LEXER_ERROR_NONE)
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
    char c;

    if (!ps_lexer_skip_whitespace_and_comments(lexer))
    {
        printf("ps_lexer_skip_whitespace_and_comments: error=%d\n", lexer->error);
        return false;
    }
    else
    {
        c = ps_lexer_peek_char(lexer);
        if (isdigit(c))
        {
            // printf("digit!\n");
            if (!ps_lexer_read_number(lexer))
                return false;
        }
        else if (isalnum(c))
        {
            // printf("alnum!\n");
            if (!ps_lexer_read_identifier_or_keyword(lexer))
                return false;
            lexer->current_token.type = ps_token_is_keyword(lexer->current_token.value.identifier);
            // printf("identifier: \"%s\" %d\n", lexer->current_token.value.identifier, lexer->current_token.type);
        }
        else if (c == '\'')
        {
            if (!ps_lexer_read_char_or_string_value(lexer))
                return false;
        }
        else
        {
            // printf("%c\n", c);
            sprintf(lexer->current_token.value.identifier, "%c", c);
            switch (c)
            {
            case ':':
                if (ps_lexer_peek_next_char(lexer) == '=')
                {
                    lexer->current_token.type = TOKEN_DOT_COLON;
                    ps_lexer_read_next_char(lexer);
                }
                else
                    lexer->current_token.type = TOKEN_COLON;
                ps_lexer_read_next_char(lexer);
                break;
            case '@':
                lexer->current_token.type = TOKEN_AT_SIGN;
                ps_lexer_read_next_char(lexer);
                break;
            case '^':
                lexer->current_token.type = TOKEN_CARET;
                ps_lexer_read_next_char(lexer);
                break;
            case ',':
                lexer->current_token.type = TOKEN_COMMA;
                ps_lexer_read_next_char(lexer);
                break;
            case '.':
                if (ps_lexer_peek_next_char(lexer) == '.')
                {
                    lexer->current_token.type = TOKEN_DOT_DOT;
                    ps_lexer_read_next_char(lexer);
                }
                else
                    lexer->current_token.type = TOKEN_DOT;
                ps_lexer_read_next_char(lexer);
                break;
            case '[':
                lexer->current_token.type = TOKEN_LEFT_BRACKET;
                ps_lexer_read_next_char(lexer);
                break;
            case '(':
                lexer->current_token.type = TOKEN_LEFT_PARENTHESIS;
                ps_lexer_read_next_char(lexer);
                break;
            case ']':
                lexer->current_token.type = TOKEN_RIGHT_BRACKET;
                ps_lexer_read_next_char(lexer);
                break;
            case ')':
                lexer->current_token.type = TOKEN_RIGHT_PARENTHESIS;
                ps_lexer_read_next_char(lexer);
                break;
            case ';':
                lexer->current_token.type = TOKEN_SEMI_COLON;
                ps_lexer_read_next_char(lexer);
                break;
            case '+':
                lexer->current_token.type = TOKEN_PLUS;
                ps_lexer_read_next_char(lexer);
                break;
            case '-':
                lexer->current_token.type = TOKEN_MINUS;
                ps_lexer_read_next_char(lexer);
                break;
            case '*':
                lexer->current_token.type = TOKEN_STAR;
                ps_lexer_read_next_char(lexer);
                break;
            case '/':
                lexer->current_token.type = TOKEN_SLASH;
                ps_lexer_read_next_char(lexer);
                break;
            case '=':
                lexer->current_token.type = TOKEN_EQUAL;
                ps_lexer_read_next_char(lexer);
                break;
            case '<':
                if (ps_lexer_peek_next_char(lexer) == '>')
                {
                    lexer->current_token.type = TOKEN_NOT_EQUAL;
                    ps_lexer_read_next_char(lexer);
                }
                else if (ps_lexer_peek_next_char(lexer) == '=')
                {
                    lexer->current_token.type = TOKEN_LESS_OR_EQUAL;
                    ps_lexer_read_next_char(lexer);
                }
                else
                    lexer->current_token.type = TOKEN_LESS_THAN;
                ps_lexer_read_next_char(lexer);
                break;
            case '>':
                if (ps_lexer_peek_next_char(lexer) == '=')
                {
                    lexer->current_token.type = TOKEN_GREATER_OR_EQUAL;
                    ps_lexer_read_next_char(lexer);
                }
                else
                    lexer->current_token.type = TOKEN_GREATER_THAN;
                ps_lexer_read_next_char(lexer);
                break;
            default:
                lexer->error = LEXER_ERROR_UNEXPECTED_CHARACTER;
            }
        }
    }
    // printf("ps_lexer_read_next_token: END\n");
    return true;
}

// ps_error_t ps_lexer_expect_token_types(lexer_t *lexer, size_t token_type_count, ps_token_type_t token_type[])

/* EOF */
